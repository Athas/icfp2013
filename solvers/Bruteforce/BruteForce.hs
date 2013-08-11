module Bruteforce.BruteForce
  ( bruteForce
  , BruteEnv
  , bruteExpFrom
  )

where

import BV

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as S

data BruteEnv = BruteEnv { envOps :: S.Set Ops
                         , envAllOps :: S.Set Ops
                         , envShrs :: Int
                         , envVars :: S.Set Id
                         , envRoom :: Int
                         }

type BruteM = ReaderT BruteEnv []

runBrute :: S.Set Ops -> Int -> BruteM a -> [a]
runBrute ops maxSize m = runReaderT m env
  where env = BruteEnv ops ops 0 (S.singleton Arg) maxSize

bruteExpFrom :: S.Set Ops -> S.Set Id -> Int -> [Exp]
bruteExpFrom ops vars room = runReaderT bruteExp $ BruteEnv ops ops 0 vars room

hasFold :: BruteM a -> BruteM a
hasFold = local $ \env -> env { envOps = TFold `S.delete` (Fold `S.delete` envOps env) }

hasIf :: BruteM a -> BruteM a
hasIf = local $ \env -> env { envOps = If0 `S.delete` envOps env }

inFold :: BruteM a -> BruteM a
inFold = local $ \env -> env { envVars = Byte `S.insert` (Acc `S.insert` envVars env) }

inTFold :: BruteM a -> BruteM a
inTFold = local $ \env -> env { envVars = Arg `S.delete`
                                          (Byte `S.insert`
                                           (Acc `S.insert` envVars env))
                              , envOps = TFold `S.delete` (Fold `S.delete` envOps env)
                              }

withUnOp :: UnOp -> BruteM Exp -> BruteM Exp
withUnOp op m = ApplyUnOp op <$> local (modEnvUnOps (opMod op) (opInvalid op)) m
  where opMod Shr1 = (+1)
        opMod Shr4 = (+4)
        opMod Shr16 = (+16)
        opMod Shl1 = \x -> x - 1
        opMod Not = id

        opInvalid Shr1 = S.fromList [UnOp Shl1]
        opInvalid Shr4 = S.fromList [UnOp Shl1, UnOp Shr1]
        opInvalid Shr16 = S.fromList [UnOp Shl1, UnOp Shr1, UnOp Shr4]
        opInvalid Shl1 = S.fromList [UnOp Shr1, UnOp Shr4, UnOp Shr16]
        opInvalid Not = S.fromList [UnOp Not]

        modEnvUnOps f invalid env =
          env { envShrs = f $ envShrs env
              , envOps = envOps env S.\\
                         case f $ envShrs env of
                           3 -> invalid `S.union` S.singleton (UnOp Shr1)
                           12 -> invalid `S.union` S.singleton (UnOp Shr4)
                           _ -> invalid
              }

clearEnvUnOps :: BruteM a -> BruteM a
clearEnvUnOps = local $ \env -> env { envOps = envOps env `S.union` S.filter isUnOp (envAllOps env) }
  where isUnOp (UnOp _) = True
        isUnOp _ = False

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
    ProgramTFold <$> inTFold (used 5 $ const bruteExp)
  else
    Program <$> used 1 (const bruteExp)

bruteExp :: BruteM Exp
bruteExp = do
  env <- ask
  case envRoom env of
    n | n < 1 -> choice []
      | otherwise ->
        let terms = map return $ Zero : One : map Var (S.toList (envVars env))
            ops = filter ((<=envRoom env) . minSize) $ S.toList $ envOps env
        in join $ choice (terms ++ map bruteOp ops)

bruteOp :: Ops -> BruteM Exp
bruteOp (UnOp op) = used 1 $ \_ ->
  withUnOp op bruteExp
bruteOp (BinOp op) = clearEnvUnOps $ used 1 $ \room -> do
  xsize <- choice [1..room `div` 2]
  ysize <- choice [room - xsize]
  x <- fill xsize bruteExp
  y <- fill ysize bruteExp
  return $ ApplyBinOp op x y
bruteOp If0 = clearEnvUnOps $ hasIf $ used 1 $ \room -> do
  e0size <- choice [1..room-2]
  e1size <- choice [1..room-1-e0size]
  e2size <- choice [1..room-e0size-e1size]
  e0 <- fill e0size bruteExp
  e1 <- fill e1size bruteExp
  e2 <- fill e2size bruteExp
  return $ If e0 e1 e2
bruteOp Fold = clearEnvUnOps $ hasFold $ used 2 $ \room -> do
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
