{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main)
where

import BV
import BruteForce

import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Word
import System.Environment
import Text.JSON

data TrainSet = TrainSet {
    trainId :: String
  , trainSize :: Int
  , trainOperators :: S.Set Ops
  , trainChallenge :: Program
  }
                deriving (Show)

runTrainSet :: Int -> TrainSet -> String
runTrainSet samples tset =
  "Testing " ++ show (trainId tset) ++ " of size " ++
  show (progSize (trainChallenge tset)) ++ ": " ++ prettyPrint (trainChallenge tset) ++ "\n" ++
  case testBruteForce (trainChallenge tset) samples (trainSize tset) (trainOperators tset) of
    [] -> "No solution found!\n"
    prog:_ -> "Found " ++ prettyPrint prog ++ "\n"


valid :: M.Map Word64 Word64 -> Program -> Bool
valid pairs prog = and [ x == output |
                         (input, output) <- M.toList pairs
                       , let x = case runProgram prog input of
                                   Left err -> error $ "Execution failed with input " ++ show input ++ ": " ++ err
                                   Right out -> out ]


testBruteForce :: Program -> Int -> Int -> S.Set Ops -> [Program]
testBruteForce prog inputs maxSize ops =
  let pairs = M.fromList [ (input, case runProgram prog input of
                                     Left err -> error $ show err
                                     Right v -> v)
                           | input <- if inputs /= 0 then
                                        [0, maxBound `div` fromIntegral inputs .. maxBound ]
                                      else [] ]
  in filter (valid pairs) $ bruteForce maxSize ops

instance JSON Ops where
  readJSON jsvalue = do
    s <- readJSON jsvalue
    case s of
      "not" -> return $ UnOp Not
      "shl1" -> return $ UnOp Shl1
      "shr1" -> return $ UnOp Shr1
      "shr4" -> return $ UnOp Shr4
      "shr16" -> return $ UnOp Shr16
      "and" -> return $ BinOp And
      "or" -> return $ BinOp Or
      "xor" -> return $ BinOp Xor
      "plus" -> return $ BinOp Plus
      "tfold" -> return TFold
      "fold" -> return Fold
      "if0" -> return If0
      _ -> fail $ "Invalid operator " ++ s

instance JSON Program where
  readJSON jsvalue = do
    prog <- parseProgram <$> readJSON jsvalue
    case prog of Right prog' -> return prog'
                 Left err    -> fail err

instance JSON TrainSet where
  readJSON jsvalue = do
    l <- fromJSObject <$> readJSON jsvalue
    case (lookup "id" l, lookup "size" l, lookup "operators" l, lookup "challenge" l) of
      (Just tid, Just size, Just ops, Just chl) -> do
        tid' <- readJSON tid
        size' <- readJSON size
        ops' <- readJSON ops
        chl' <- readJSON chl
        return TrainSet { trainId = tid', trainSize = size',
                          trainOperators = ops', trainChallenge = chl'
                        }
      (Nothing, _, _, _) -> fail "Missing id field"
      (_, Nothing, _, _) -> fail "Missing size field"
      (_, _, Nothing, _) -> fail "Missing operators field"
      (_, _, _, Nothing) -> fail "Missing challenge field"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bruteforce", n] ->
      let procLine line = case decode line of
                            Error err -> error $ "Invalid input line: " ++ err
                            Ok v -> runTrainSet (read n) v
      in interact $ unlines . map procLine . lines
    _ -> error "Wrong options"
