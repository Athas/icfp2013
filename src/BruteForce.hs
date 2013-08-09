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
                         , envFilling :: Bool
                         }

type BruteM = ReaderT BruteEnv []

runBrute :: S.Set Ops -> Int -> BruteM a -> [a]
runBrute ops maxSize m = runReaderT m env
  where env = BruteEnv ops (S.singleton Arg) maxSize True

unOp :: Ops -> BruteM a -> BruteM a
unOp op = local (\env -> env { envOps = op `S.delete` envOps env })

used :: Int -> BruteM a -> BruteM a
used k = local (\env -> env { envRoom = envRoom env - k })

unfilling :: BruteM a -> BruteM a
unfilling = local (\env -> env { envFilling = False })

filling :: BruteM a -> BruteM a
filling = local (\env -> env { envFilling = envFilling env && True })

choice :: [a] -> BruteM a
choice = lift

valid :: M.Map Word64 Word64 -> Program -> Bool
valid pairs prog = and [ x == output |
                         (input, output) <- M.toList pairs
                       , let Right x = runProgram prog input ]

bruteForce :: Int -> M.Map Word64 Word64 -> S.Set Ops -> [Program]
bruteForce size pairs ops = filter (valid pairs) $ runBrute ops size $ do
  if TFold `S.member` ops then do
    body <- unOp TFold $ used 4 bruteExp
    return $ Program (ApplyFold (Var Arg) Zero body)
  else
    Program <$> used 1 bruteExp

bruteExp :: BruteM Exp
bruteExp = do
  env <- ask
  let leaves = Zero : One : map Var (S.toList (envVars env))
  case envRoom env of
    n | n < 1 -> choice []
      | n == 1 -> choice leaves
      | otherwise ->
        let leaves' = if envFilling env then [] else leaves in
        case filter ((<=envRoom env) . minSize) $ S.toList $ envOps env of
          [] -> choice leaves'
          ops -> do act <- choice $ map return leaves' ++ map bruteOp ops
                    act

bruteOp :: Ops -> BruteM Exp
bruteOp (UnOp op) = ApplyUnOp op <$> used 1 bruteExp
bruteOp (BinOp op) = do
  x <- used 1 $ unfilling bruteExp
  y <- used (1+expSize x) $ filling bruteExp
  return $ ApplyBinOp op x y
bruteOp If0 = do
  e0 <- used 1 $ unfilling bruteExp
  e1 <- used (1+expSize e0) $ unfilling bruteExp
  e2 <- used (1+expSize e0+expSize e1) $ filling bruteExp
  return $ If e0 e1 e2

minSize :: Ops -> Int
minSize (UnOp _) = 2
minSize (BinOp _) = 3
minSize Fold = 5
minSize TFold = 5
minSize If0 = 4
