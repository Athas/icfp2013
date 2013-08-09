module BruteForce
  ( bruteForce
  )

where

import BV

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Data.Word
import qualified Data.Map as M
import qualified Data.Set as S

data BruteEnv = BruteEnv { envOps :: S.Set Ops
                         , envVars :: S.Set Id
                         , envRoom :: Int
                         }

type BruteM = ReaderT BruteEnv []

runBrute :: S.Set Ops -> Int -> BruteM a -> [a]
runBrute ops maxSize m = runReaderT m env
  where env = BruteEnv ops (S.singleton Arg) maxSize

hasFold :: BruteM a -> BruteM a
hasFold = local (\env -> env { envOps = TFold `S.delete` (Fold `S.delete` envOps env) })

inFold :: BruteM a -> BruteM a
inFold = local (\env -> env { envVars =  Byte `S.insert` (Acc `S.insert` envVars env) })

used :: Int -> (Int -> BruteM a) -> BruteM a
used k m = local (\env -> env { envRoom = envRoom env - k }) $
           m =<< asks envRoom

fill :: Int -> BruteM a -> BruteM a
fill n = local (\env -> env { envRoom = n })

choice :: [a] -> BruteM a
choice = lift

bruteForce :: Int -> S.Set Ops -> [Program]
bruteForce size ops = runBrute ops size $
  if TFold `S.member` ops then do
    body <- hasFold $ inFold $ used 5 $ const bruteExp
    return $ Program (ApplyFold (Var Arg) Zero body)
  else
    Program <$> used 1 (const bruteExp)

bruteExp :: BruteM Exp
bruteExp = do
  env <- ask
  case envRoom env of
    n | n < 1 -> choice []
      | n == 1 -> choice $ Zero : One : map Var (S.toList (envVars env))
      | otherwise ->
        case filter ((<=envRoom env) . minSize) $ S.toList $ envOps env of
          ops -> join $ choice (map bruteOp ops)

bruteOp :: Ops -> BruteM Exp
bruteOp (UnOp op) = used 1 $ \room ->
  ApplyUnOp op <$> fill room bruteExp
bruteOp (BinOp op) = used 1 $ \room -> do
  xsize <- choice [1..room `div` 2]
  ysize <- choice [room - xsize]
  x <- fill xsize bruteExp
  y <- fill ysize bruteExp
  return $ ApplyBinOp op x y
bruteOp If0 = used 1 $ \room -> do
  e0size <- choice [1..room-2]
  e1size <- choice [1..room-1-e0size]
  e2size <- choice [1..room-e0size-e1size]
  e0 <- fill e0size bruteExp
  e1 <- fill e1size bruteExp
  e2 <- fill e2size bruteExp
  return $ If e0 e1 e2
bruteOp Fold = hasFold $ used 2 $ \room -> do
  e0size <- choice [1..room-2]
  e1size <- choice [1..room-1-e0size]
  e2size <- choice [1..room-e0size-e1size]
  e0 <- fill e0size $ bruteExp
  e1 <- fill e1size bruteExp
  e2 <- fill e2size $ inFold bruteExp
  return $ ApplyFold e0 e1 e2
bruteOp TFold = error "Unexpected TFold"

minSize :: Ops -> Int
minSize (UnOp _) = 2
minSize (BinOp _) = 3
minSize Fold = 5
minSize TFold = 5
minSize If0 = 4
