module Bruteforce.KillAllPolarBears
  ( bruteForce
  )

where

import BV
import Bruteforce.BruteForce (bruteExpFrom)

import Control.Monad.State
import Control.Monad.Par (Par)
import qualified Control.Monad.Par as Par
import Control.Monad.Reader
import Control.Monad.List

import qualified Data.Map as M
import qualified Data.Set as S

data BruteEnv = BruteEnv { envOps :: S.Set Ops
                         , envVars :: S.Set Id
                         , envRoom :: Int
                         , envParThreshold :: Int
                         }

data BruteState = BruteState { stateFutures :: M.Map Int Result
                             , stateI :: Int
                             }

type Result = BruteM [Exp]

type BruteM = ReaderT BruteEnv (StateT BruteState (ListT Par))

runBrute :: S.Set Ops -> Int -> BruteM a -> [a]
runBrute ops maxSize m = Par.runPar $ runListT $ evalStateT (runReaderT m env) s
  where env = BruteEnv ops (S.singleton Arg) maxSize (min 10 (maxSize `div` 2))
        s = BruteState M.empty 0

hasFold :: BruteM a -> BruteM a
hasFold = local (\env -> env { envOps = TFold `S.delete` (Fold `S.delete` envOps env) })

hasIf :: BruteM a -> BruteM a
hasIf = local $ \env -> env { envOps = If0 `S.delete` envOps env }

inFold :: BruteM a -> BruteM a
inFold = local (\env -> env { envVars =  Byte `S.insert` (Acc `S.insert` envVars env) })

inTFold :: BruteM a -> BruteM a
inTFold = local $ \env -> env { envVars = Arg `S.delete`
                                          (Byte `S.insert`
                                           (Acc `S.insert` envVars env))
                              , envOps = TFold `S.delete` (Fold `S.delete` envOps env)
                              }

used :: Int -> (Int -> BruteM a) -> BruteM a
used k m = local (\env -> env { envRoom = envRoom env - k }) $
           m =<< asks envRoom

fill :: Int -> BruteM a -> BruteM a
fill n = local (\env -> env { envRoom = n })

choice :: [a] -> BruteM a
choice = lift . lift . ListT . return

liftPar :: Par a -> BruteM a
liftPar = lift . lift . lift

forkOff :: [Exp] -> BruteM Int
forkOff m = do
  i <- gets stateI
  var <- liftPar $ Par.spawn $ return m
  modify $ \s -> s { stateFutures = M.insert i (liftPar $ Par.get var) $ stateFutures s
                   , stateI = i + 1
                   }
  return i

forkSubtree :: BruteM Result
forkSubtree = do
  env <- ask
  i <- forkOff $ bruteExpFrom (envOps env) (envVars env) (envRoom env)
  return $ do var <- gets $ M.lookup i . stateFutures
              case var of Nothing -> error $ "Unknown ID " ++ show i
                          Just var' -> var'

bruteForce :: Int -> S.Set Ops -> [Program]
bruteForce size ops = concat $ runBrute ops size $
  if TFold `S.member` ops then do
    exps <- join $ inTFold $ used 5 $ const bruteExp
    return $ map ProgramTFold exps
  else do
    body <- join $ used 1 (const bruteExp)
    return $ map Program body

bruteExp :: BruteM Result
bruteExp = do
  env <- ask
  case envRoom env of
    n | n <= envParThreshold env -> forkSubtree
      | n < 1 -> choice []
      | n == 1 -> choice [return $ Zero : One : map Var (S.toList (envVars env))]
      | otherwise ->
        case filter ((<=envRoom env) . minSize) $ S.toList $ envOps env of
          ops -> join $ choice (map bruteOp ops)

bruteOp :: Ops -> BruteM Result
bruteOp (UnOp op) = used 1 $ \room -> do
  res <- fill room bruteExp
  return $ do es <- res
              return $ map (ApplyUnOp op) es
bruteOp (BinOp op) = used 1 $ \room -> do
  xsize <- choice [1..room `div` 2]
  ysize <- choice [room - xsize]
  xres <- fill xsize bruteExp
  yres <- fill ysize bruteExp
  return $ do xs <- xres
              ys <- yres
              return [ ApplyBinOp op x y | x <- xs, y <- ys ]
bruteOp If0 = hasIf $ used 1 $ \room -> do
  e0size <- choice [1..room-2]
  e1size <- choice [1..room-1-e0size]
  e2size <- choice [1..room-e0size-e1size]
  e0res <- fill e0size bruteExp
  e1res <- fill e1size bruteExp
  e2res <- fill e2size bruteExp
  return $ do e0s <- e0res
              e1s <- e1res
              e2s <- e2res
              return [ If e0 e1 e2 | e0 <- e0s, e1 <- e1s, e2 <- e2s ]
bruteOp Fold = hasFold $ used 2 $ \room -> do
  e0size <- choice [1..room-2]
  e1size <- choice [1..room-1-e0size]
  e2size <- choice [1..room-e0size-e1size]
  e0res <- fill e0size $ bruteExp
  e1res <- fill e1size bruteExp
  e2res <- fill e2size $ inFold bruteExp
  return $ do e0s <- e0res
              e1s <- e1res
              e2s <- e2res
              return [ ApplyFold e0 e1 e2 | e0 <- e0s, e1 <- e1s, e2 <- e2s ]
bruteOp TFold = error "Unexpeacted TFold"

minSize :: Ops -> Int
minSize (UnOp _) = 2
minSize (BinOp _) = 3
minSize Fold = 5
minSize TFold = 5
minSize If0 = 4
