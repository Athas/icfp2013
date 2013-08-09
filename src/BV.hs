module BV
  ( Program(..)
  , Id(..)
  , Exp(..)
  , UnOp(..)
  , BinOp(..)
  , prettyPrint
  , parseProgram
  , runProgram
  , progSize
  , expSize
  , Ops(..)
  , expOperators
  , operators
  )
    where

import Control.Applicative
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits
import Data.Word

import Text.Parsec hiding (many, token, (<|>))
import Text.Parsec.String

data Id = Arg | Byte | Acc
        deriving (Eq, Ord, Show)

data Program = Program Exp
               deriving (Eq, Show)

data Exp = Zero
         | One
         | Var Id
         | If Exp Exp Exp
         | ApplyFold Exp Exp Exp
         | ApplyUnOp UnOp Exp
         | ApplyBinOp BinOp Exp Exp
           deriving (Eq, Show)

data UnOp = Not
          | Shl1
          | Shr1
          | Shr4
          | Shr16
            deriving (Eq, Ord, Show)

data BinOp = And
           | Or
           | Xor
           | Plus
             deriving (Eq, Ord, Show)

prettyPrint :: Program -> String
prettyPrint (Program e) =
  "(lambda (" ++ ppId Arg ++ ") " ++ ppExp e ++ ")"

ppId :: Id -> String
ppId Arg = "arg"
ppId Byte = "byte"
ppId Acc = "acc"

ppExp :: Exp -> String
ppExp Zero = "0"
ppExp One = "1"
ppExp (Var k) = ppId k
ppExp (If e1 e2 e3) =
  "(if0 " ++ ppExp e1 ++ " " ++ ppExp e2 ++ " " ++ ppExp e3 ++ ")"
ppExp (ApplyFold e1 e2 e3) =
    "(fold " ++ ppExp e1 ++ " " ++ ppExp e2 ++
    " (" ++ ppId Byte ++ " " ++ ppId Acc ++ ") " ++ ppExp e3 ++ ")"
ppExp (ApplyUnOp op e) = "(" ++ ppUnOp op ++ " " ++ ppExp e ++ ")"
ppExp (ApplyBinOp op e1 e2) =
    "(" ++ ppBinOp op ++ " " ++ ppExp e1 ++ " " ++ ppExp e2 ++ ")"

ppUnOp :: UnOp -> String
ppUnOp Not = "not"
ppUnOp Shl1 = "shl1"
ppUnOp Shr1 = "shr1"
ppUnOp Shr4 = "shr4"
ppUnOp Shr16 = "shr16"

ppBinOp :: BinOp -> String
ppBinOp And = "and"
ppBinOp Or = "or"
ppBinOp Xor = "xor"
ppBinOp Plus = "plus"

parseProgram :: String -> Either String Program
parseProgram = either (Left . show) Right . parse parseBV "input"

constituent :: Parser Char
constituent = oneOf $ ['a'..'z']++"_"++['0'..'9']

token :: String -> Parser ()
token s = string s *> spaces

keyword :: String -> Parser ()
keyword s = try $ string s *> notFollowedBy constituent *> spaces

parens :: Parser a -> Parser a
parens = between (token "(") (token ")")

parseName :: Parser String
parseName = (:) <$> oneOf ['a'..'z'] <*> many constituent <* spaces

parseId :: M.Map String Id -> Parser Id
parseId ns = do s <- parseName
                case M.lookup s ns of
                  Nothing -> error $ "Unknown variable " ++ s ++ " " ++ show ns
                  Just s' -> return s'

parseBV :: Parser Program
parseBV = spaces *> parens body
  where body = do keyword "lambda"
                  arg <- parens parseName
                  Program <$> parseExp (M.singleton arg Arg)

parseExp :: M.Map String Id -> Parser Exp
parseExp ns = token "0" *> pure Zero <|>
              token "1" *> pure One <|>
              Var <$> parseId ns <|>
              parens (keyword "if0" *> pure If <*> parseExp ns <*> parseExp ns <*> parseExp ns <|>
                      keyword "fold" *> pure ApplyFold <*> parseExp ns <*> parseExp ns <*> parseFoldLambda <|>
                      pure ApplyUnOp <*> parseUnOp <*> parseExp ns <|>
                      pure ApplyBinOp <*> parseBinOp <*> parseExp ns <*> parseExp ns)
  where parseFoldLambda = parens $ do
          keyword "lambda"
          (byte, acc) <- parens $ (,) <$> parseName <*> parseName
          parseExp (M.insert byte Byte $ M.insert acc Acc ns)

parseUnOp :: Parser UnOp
parseUnOp = keyword "not" *> pure Not <|>
            keyword "shl1" *> pure Shl1 <|>
            keyword "shr1" *> pure Shr1 <|>
            keyword "shr4" *> pure Shr4 <|>
            keyword "shr16" *> pure Shr16

parseBinOp :: Parser BinOp
parseBinOp = keyword "and" *> pure And <|>
             keyword "or" *> pure Or <|>
             keyword "xor" *> pure Xor <|>
             keyword "plus" *> pure Plus

runProgram :: Program -> Word64 -> Either String Word64
runProgram (Program progbody) arg =
  runReaderT (evalExp progbody) $ M.singleton Arg arg
  where evalExp One = return 1
        evalExp Zero = return 0
        evalExp (Var k) = do
          x <- asks $ M.lookup k
          case x of Nothing -> lift $ Left $ "Unbound variable " ++ ppId k
                    Just x' -> return x'
        evalExp (If c x y) = do
          c' <- evalExp c
          evalExp $ if c' == 0 then x else y
        evalExp (ApplyFold inp inacc body) = do
          inp' <- evalExp inp
          inacc' <- evalExp inacc
          let bytes = map ((.&. 0xFF) . (inp' `shiftR`))
                      [0, 8, 16, 24, 32, 40, 48, 56]
              comb accv bytev =
                local (M.union (M.fromList [(Byte, bytev), (Acc, accv)])) $
                  evalExp body
          foldM comb inacc' bytes
        evalExp (ApplyUnOp Not e) =
          complement <$> evalExp e
        evalExp (ApplyUnOp Shl1 e) =
          (`shiftL` 1) <$> evalExp e
        evalExp (ApplyUnOp Shr1 e) =
          (`shiftR` 1) <$> evalExp e
        evalExp (ApplyUnOp Shr4 e) =
          (`shiftR` 4) <$> evalExp e
        evalExp (ApplyUnOp Shr16 e) =
          (`shiftR` 16) <$> evalExp e
        evalExp (ApplyBinOp And e1 e2) =
          pure (.&.) <*> evalExp e1 <*> evalExp e2
        evalExp (ApplyBinOp Or e1 e2) =
          pure (.|.) <*> evalExp e1 <*> evalExp e2
        evalExp (ApplyBinOp Xor e1 e2) =
          pure xor <*> evalExp e1 <*> evalExp e2
        evalExp (ApplyBinOp Plus e1 e2) =
          pure (+) <*> evalExp e1 <*> evalExp e2

progSize :: Program -> Int
progSize (Program body) = 1 + expSize body

expSize :: Exp -> Int
expSize Zero = 1
expSize One = 1
expSize (Var _) = 1
expSize (If e0 e1 e2) = 1 + expSize e0 + expSize e1 + expSize e2
expSize (ApplyFold e0 e1 e2) = 2 + expSize e0 + expSize e1 + expSize e2
expSize (ApplyUnOp _ e0) = 1 + expSize e0
expSize (ApplyBinOp _ e0 e1) = 1 + expSize e0 + expSize e1

data Ops = UnOp UnOp
         | BinOp BinOp
         | Fold
         | TFold
         | If0
           deriving (Eq, Ord, Show)

expOperators :: Exp -> S.Set Ops
expOperators Zero = S.empty
expOperators One = S.empty
expOperators (Var _) = S.empty
expOperators (If e0 e1 e2) = S.insert If0 $ S.unions $ map expOperators [e0, e1, e2]
expOperators (ApplyFold e0 e1 e2) = S.insert Fold $ S.unions $ map expOperators [e0, e1, e2]
expOperators (ApplyUnOp op e) = S.insert (UnOp op) $ expOperators e
expOperators (ApplyBinOp op e0 e1) = S.insert (BinOp op) $ S.unions $ map expOperators [e0, e1]

operators :: Program -> S.Set Ops
operators (Program (ApplyFold (Var Arg) Zero e)) =
  S.insert TFold (expOperators e)
operators (Program e) = expOperators e
