{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main)
where

import BV
import qualified BruteForce as TroelsForce
import qualified DybberBruteForce as DybberForce

import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Word
import System.Environment
import Text.Printf
import Text.JSON

data TrainSet = TrainSet {
    trainId :: String
  , trainSize :: Int
  , trainOperators :: S.Set Ops
  , trainChallenge :: Program
  }
                deriving (Show)

runTrainSet :: (Int -> S.Set Ops -> [Program]) -> Int -> TrainSet -> String
runTrainSet bf samples tset =
  "Testing " ++ show (trainId tset) ++ " of size " ++
  show (progSize (trainChallenge tset)) ++ ": " ++ prettyPrint (trainChallenge tset) ++ "\n" ++
  case testBruteForce bf (trainChallenge tset) samples (trainSize tset) (trainOperators tset) of
    [] -> error $ "No solution found!\n"
    prog:_ -> "Found " ++ prettyPrint prog ++ "\n"


valid :: M.Map Word64 Word64 -> Program -> Bool
valid pairs prog = and [ x == output |
                         (input, output) <- M.toList pairs
                       , let x = case runProgram prog input of
                                   Left err -> error $ "Execution failed with input " ++ show input ++ ": " ++ err
                                   Right out -> out ]


testBruteForce :: (Int -> S.Set Ops -> [Program])
               -> Program -> Int -> Int -> S.Set Ops -> [Program]
testBruteForce bf prog inputs maxSize ops =
  let pairs = M.fromList [ (input, case runProgram prog input of
                                     Left err -> error $ show err
                                     Right v -> v)
                           | input <- if inputs /= 0 then
                                        [0, maxBound `div` fromIntegral inputs .. maxBound ]
                                      else [] ]
  in filter (valid pairs) $ bf maxSize ops

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

bruteForce :: (Int -> S.Set Ops -> [Program]) -> Int -> IO ()
bruteForce bf n =
  let procLine line = case decode line of
                        Error err -> error $ "Invalid input line: " ++ err
                        Ok v -> runTrainSet bf n v
  in interact $ unlines . map procLine . lines

solve :: (Int -> S.Set Ops -> [Program])
      -> String -> String -> String -> IO ()
solve bf sizestr opsstr iosstr =
  case (reads sizestr, decode opsstr, reads iosstr) of
    ([(size, [])], Ok ops, [(ios, [])]) ->
      case filter (valid $ M.fromList ios) $ bf size (S.fromList ops) of
        [] -> error "No solution found"
        prog:_ -> putStrLn $ prettyPrint prog
    (_, Error err, _) -> error err
    (_, _, _) -> error "Malformed input"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bruteforce", n] -> bruteForce TroelsForce.bruteForce (read n)
    ["dybberforce", n] -> bruteForce DybberForce.bruteForce (read n)
    -- Example:
    -- ./Main solve 11 '["shr1","shr4","shr16","or","tfold"]' '[(1,1),(2,2),(3,3)]'
    ["solve", sizestr, opsstr, iosstr] ->
      solve TroelsForce.bruteForce sizestr opsstr iosstr
    ["dybbersolve", sizestr, opsstr, iosstr] ->
      solve DybberForce.bruteForce sizestr opsstr iosstr
    ["eval", progstr, inputstr] -> do
       case (parseProgram progstr, reads inputstr) of
         (Left err, _) -> error err
         (Right prog, [(input, [])]) ->
           case runProgram prog input of
             Right v -> printf "0x%x\n" v
             Left err -> error err
         (_, _) -> error "Invalid numeric input"
    _ -> error "Wrong options"
