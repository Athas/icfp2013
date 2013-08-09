module Main where

import System.Environment
import qualified System.Process as P
import Data.Word
import BV

fromRight = either error id

plotProgram :: Program -> Int -> IO ()
plotProgram program npairs = do
  _ <- P.createProcess $ P.proc "./src/plot.py"
       (prettyPrint program : map show pairs)
  return ()
  where xs = [0, maxBound `div` fromIntegral npairs..maxBound]
        pairs = map (\x -> (x, fromRight $ runProgram program x)) xs

main :: IO ()
main = do
  [program, npairs] <- getArgs
  plotProgram (fromRight $ parseProgram program) (read npairs)
