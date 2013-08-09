module BV
  ( Program(..)
  , Id
  , Exp(..)
  , UnOp(..)
  , BinOp(..)
  , prettyPrint
  , parseProgram
  )
    where

import Control.Applicative

import Text.Parsec hiding (many, token, (<|>))
import Text.Parsec.String

type Id = String

data Program = Program Id Exp

data Exp = Zero
         | One
         | Var Id
         | If Exp Exp Exp
         | Fold Exp Exp (Id, Id) Exp
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
ppExp (Fold e1 e2 (k1, k2) e3) =
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
                   token "fold" *> pure Fold <*> parseExp <*> parseExp <*> parens (pure (,) <*> parseId <*> parseId) <*> parseExp <|>
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
