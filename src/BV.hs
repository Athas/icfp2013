module BV
  ( Program(..)
  , Id
  , Exp(..)
  , UnOp(..)
  , BinOp(..)
  , prettyPrint
  , parseProgram
  , runProgram
  )
    where

import Control.Applicative
import Control.Monad.Reader

import qualified Data.Map as M
import Data.Bits
import Data.Word

import Text.Parsec hiding (many, token, (<|>))
import Text.Parsec.String

type Id = String

data Program = Program Id Exp

data Exp = Zero
         | One
         | Var Id
         | If Exp Exp Exp
         | Fold Exp Exp ((Id, Id), Exp)
         | UnOp UnOp Exp
         | BinOp BinOp Exp Exp

data UnOp = Not
          | Shl1
          | Shr1
          | Shr4
          | Shr16

data BinOp = And
           | Or
           | Xor
           | Plus

prettyPrint :: Program -> String
prettyPrint (Program k e) =
  "(lambda (" ++ k ++ ") " ++ ppExp e ++ ")"

ppExp :: Exp -> String
ppExp Zero = "0"
ppExp One = "1"
ppExp (Var k) = k
ppExp (If e1 e2 e3) =
  "(if " ++ ppExp e1 ++ " " ++ ppExp e2 ++ " " ++ ppExp e3 ++ ")"
ppExp (Fold e1 e2 ((k1, k2), e3)) =
    "(fold " ++ ppExp e1 ++ " " ++ ppExp e2 ++
    " (" ++ k1 ++ " " ++ k2 ++ ") " ++ ppExp e3 ++ ")"
ppExp (UnOp op e) = "(" ++ ppUnOp op ++ " " ++ ppExp e ++ ")"
ppExp (BinOp op e1 e2) =
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

token :: String -> Parser ()
token s = string s *> spaces

parens :: Parser a -> Parser a
parens = between (token "(") (token ")")

parseId :: Parser Id
parseId = (:) <$> oneOf ['a'..'z'] <*> many (oneOf $ ['a'..'z']++"_"++['0'..'9']) <* spaces

parseBV :: Parser Program
parseBV = spaces *> parens body
  where body = Program <$>
               (token "lambda" *> parens parseId) <*>
               parseExp

parseExp :: Parser Exp
parseExp = token "0" *> pure Zero <|>
           token "1" *> pure One <|>
           Var <$> parseId <|>
           parens (token "if" *> pure If <*> parseExp <*> parseExp <*> parseExp <|>
                   token "fold" *> pure Fold <*> parseExp <*> parseExp <*> parens (token "lambda" *> pure (,) <*> parens (pure (,) <*> parseId <*> parseId) <*> parseExp) <|>
                   pure UnOp <*> parseUnOp <*> parseExp <|>
                   pure BinOp <*> parseBinOp <*> parseExp <*> parseExp)

parseUnOp :: Parser UnOp
parseUnOp = token "not" *> pure Not <|>
            token "shl1" *> pure Shl1 <|>
            token "shr1" *> pure Shr1 <|>
            token "shr4" *> pure Shr4 <|>
            token "shr16" *> pure Shr16

parseBinOp :: Parser BinOp
parseBinOp = token "and" *> pure And <|>
             token "or" *> pure Or <|>
             token "xor" *> pure Xor <|>
             token "plus" *> pure Plus

runProgram :: Program -> Word64 -> Either String Word64
runProgram (Program k0 progbody) arg =
  runReaderT (evalExp progbody) $ M.singleton k0 arg
  where evalExp One = return 1
        evalExp Zero = return 0
        evalExp (Var k) = do
          x <- asks $ M.lookup k
          case x of Nothing -> lift $ Left $ "Unbound variable " ++ k
                    Just x' -> return x'
        evalExp (If c x y) = do
          c' <- evalExp c
          evalExp $ if c' /= 0 then x else y
        evalExp (Fold inp inacc ((byte, acc), body)) = do
          inp' <- evalExp inp
          inacc' <- evalExp inacc
          let bytes = map ((.&. 0xFF) . (inp' `shiftR`))
                      [0, 8, 16, 24, 32, 40, 48, 56]
              comb accv bytev =
                local (M.union (M.fromList [(byte, bytev), (acc, accv)])) $
                  evalExp body
          foldM comb inacc' bytes
        evalExp (UnOp Not e) =
          complement <$> evalExp e
        evalExp (UnOp Shl1 e) =
          (`shiftL` 1) <$> evalExp e
        evalExp (UnOp Shr1 e) =
          (`shiftR` 1) <$> evalExp e
        evalExp (UnOp Shr4 e) =
          (`shiftR` 4) <$> evalExp e
        evalExp (UnOp Shr16 e) =
          (`shiftR` 16) <$> evalExp e
        evalExp (BinOp And e1 e2) =
          pure (.&.) <*> evalExp e1 <*> evalExp e2
        evalExp (BinOp Or e1 e2) =
          pure (.|.) <*> evalExp e1 <*> evalExp e2
        evalExp (BinOp Xor e1 e2) =
          pure xor <*> evalExp e1 <*> evalExp e2
        evalExp (BinOp Plus e1 e2) =
          pure (+) <*> evalExp e1 <*> evalExp e2
