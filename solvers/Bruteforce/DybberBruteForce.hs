module Bruteforce.DybberBruteForce where

import BV
import qualified Data.Set as S

genExp :: Int -> [Ops] -> Bool -> [Exp]
genExp _ [] _ = []
genExp 1 _ inFold = [Zero, One, Var Arg] ++ if inFold then [Var Byte, Var Acc] else []
genExp size ops inFold = concatMap (useOp size ops inFold) ops 

useOp size ops inFold (UnOp op) = map (ApplyUnOp op) (genExp (size-1) ops inFold)
useOp size ops inFold (BinOp op) | size < 3 = []
                                 | otherwise = 
                   [ApplyBinOp op x1 x2 | s1 <- [1..(size - 1) `div` 2]
                                        , let s2 = size - 1 - s1
                                        , x1 <- genExp s1 ops inFold
                                        , x2 <- genExp s2 ops inFold]
useOp size ops inFold If0 | size < 4 = []
                          | otherwise = [If b x1 x2 | s_b <- [1..size-3]
                                        , s_x1 <- [1..size-2-s_b]
                                        , let s_x2 = size - 2 - s_x1
                                        , b <- genExp s_b ops inFold
                                        , x1 <- genExp s_x1 ops inFold
                                        , x2 <- genExp s_x2 ops inFold]
useOp size ops inFold Fold | size < 5 = []
                           | otherwise =
                   [ApplyFold x1 x2 x3 | s1 <- [1..size-4]
                                       , s2 <- [1..size-4-s1]
                                       , let s3 = size - 4 - s2
                                       , x1 <- genExp s1 (removeFold ops) inFold
                                       , x2 <- genExp s1 (removeFold ops) inFold
                                       , x3 <- genExp s2 (removeFold ops) True]

removeFold = filter (\x -> x /= Fold && x /= TFold)

bruteForce :: Int -> S.Set Ops -> [Program]
bruteForce size ops = 
  if TFold `S.member` ops
  then map (\body -> Program (ApplyFold (Var Arg) Zero body))
           (genExp (size-5) (removeFold o) True)
  else map Program (genExp (size-1) o False)
 where
   o = S.toList ops
