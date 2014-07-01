{-#
  LANGUAGE
    FlexibleInstances,
    FlexibleContexts,
    MultiParamTypeClasses,
    RankNTypes,
    GADTs
  #-}
module Main where

import Control.Monad.FreeReflect
import Control.Monad.State.Class

import Test.HUnit

import qualified Data.TypedSequence as Seq

data S s a = S { unS :: s -> (s, a) }

instance Functor (S s) where
  fmap f (S h) = S (fmap f . h)

-- S s (Free (S s) a) == s -> (s, Free (S s) a)

instance MonadState s (Free (S s)) where
  state f = fromView (Step $ S $ \st -> let
                         (x, st') = f st
                         in
                          (st', return x))

instance MonadState s (Program (S s)) where
  state f = primitive $ S (swap . f)
    where swap ~(x,y) = (y,x)

runFreeState :: Free (S s) a -> s -> (a, s)
runFreeState m = go (toView m)
  where
    go (Pure x) st = (x, st)
    go (Step (S h)) st = let
      (st', m') = h st
      in
       go (toView m') st'

runProgramState :: Program (S s) a -> s -> (a, s)
runProgramState (Return x) st = (x, st)
runProgramState (Primitive (S h) k) st = let
  (st', x) = h st
  rest = unKA (Seq.foldl k) x -- Program (S s) b
  in
   runProgramState rest st'

data LCMS a = LCMS {
  lcmsBestLen :: Int,
  lcmsBest :: [a],
  lcmsCurrLen :: Int,
  lcmsCurr :: [a]
  }
            deriving Show

computeLCMS :: (Ord a, MonadState (LCMS a) m) => [a] -> m ()
computeLCMS [] = return ()
computeLCMS (x:xs) = do
  my <- currHead
  case my of
    Nothing -> pushCurr x
    Just y | x >= y -> pushCurr x
           | otherwise -> resetCurr
  updateBest
  computeLCMS xs
  where
    currHead = get >>= \LCMS {lcmsCurr = ys} ->
      return $ case ys of
        [] -> Nothing
        (y:_) -> Just y
    pushCurr x = modify $ \st -> st { lcmsCurrLen = 1 + lcmsCurrLen st
                                    , lcmsCurr = x : lcmsCurr st
                                    }
    resetCurr = modify $ \st -> st { lcmsCurrLen = 0
                                   , lcmsCurr = []
                                   }
    updateBest = modify $ \st -> if lcmsCurrLen st > lcmsBestLen st
                                 then st { lcmsBestLen = lcmsCurrLen st
                                         , lcmsBest = lcmsCurr st
                                         }
                                 else st
    
initLCMS :: LCMS a
initLCMS = LCMS { lcmsCurrLen = 0, lcmsCurr = []
                , lcmsBestLen = 0, lcmsBest = []
                }

lcms :: (Ord a, MonadState (LCMS a) m) => (forall b . m b -> LCMS a -> (b, LCMS a)) -> [a] -> [a]
lcms runner xs = let
  (_, LCMS { lcmsBest = ys}) = runner (computeLCMS xs) initLCMS 
  in
   reverse ys

main = do
  let
    xs = [2,3,4,-1,5,6,7,8,-2,5::Int]
    ys1 = lcms runProgramState xs
    ys2 = lcms runFreeState xs
    ys1' = lcms (runFreeState . fwd . back) xs
  _ <- runTestTT $ TestList [
    "Longest Continuous Monotonic Subsequence" ~: assertEqual "" ys1 ys2
    , "LCMS (roundtrip)" ~: assertEqual "" ys1 ys1'
    ]
  return ()
