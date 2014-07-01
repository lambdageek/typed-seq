{-#
  LANGUAGE
    GADTs
  , ScopedTypeVariables
  #-}
module Main where

import Control.Applicative
import Control.Monad
import qualified Data.TypedSequence as S
import Control.Monad.FreeReflect

import Test.HUnit

data Pair a = Pair (a,a)

instance Show a => Show (Pair a) where
  showsPrec _p (Pair (x, y)) = parens (shows x . showString " | " . shows y)
    where
      parens f = showString "(" . f . showString ")"
  
instance Functor Pair where
  fmap f (Pair (x, y)) = Pair (f x, f y)

newtype Tree a = Tree { unTree :: Free Pair a }

instance Show a => Show (Tree a) where
  showsPrec _ (Tree t) = case toView t of
    Pure a -> shows a
    Step p -> shows (fmap Tree p)

singleton :: a -> Tree a
singleton x = Tree (fromView $ Pure x)

fromPairTree :: Pair (Tree a) -> Tree a
fromPairTree = Tree . fromView . Step . fmap unTree 

mkTree :: Tree a -> Tree a -> Tree a
mkTree t1 t2 = fromPairTree $ Pair (t1, t2)

viewMin :: Tree a -> a
viewMin (Tree t) = case toView t of
  Pure x -> x
  Step (Pair (t1, _)) -> viewMin (Tree t1)

insert :: Ord a => a -> Tree a -> Tree a
insert x (Tree t) = case toView t of
  Pure y -> (case compare x y of
                LT -> mkTree (singleton x) (singleton y)
                GT -> mkTree (singleton y) (singleton x)
                EQ -> singleton y)
  Step (Pair (t1, t2)) ->
    (case compare x (viewMin (Tree t2)) of
        LT -> mkTree (insert x (Tree t1)) (Tree t2)
        EQ -> mkTree (Tree t1) (Tree t2)
        GT -> mkTree (Tree t1) (insert x (Tree t2)))

round1 :: Tree a -> Tree a
round1 = Tree . fwd . back . unTree

tree1 :: Tree String
tree1 = insert "r" $ insert "a" $ insert "z" $ insert "y" $ singleton "x"
-- round1 tree1 == tree1

type RPair = ReifiedFunctor Pair

newtype Subst a = Subst {unSubst :: Program RPair a}

instance Functor Subst where
  fmap = liftM

instance Applicative Subst where
  pure = return
  (<*>) = ap

instance Monad Subst where
  return = Subst . return
  (Subst m) >>= f = Subst (m >>= unSubst . f)

instance Show a => Show (Subst a) where
  showsPrec _ (Subst (Return x)) = shows x
  showsPrec _ (Subst (Primitive (FMap f p) k)) = let
    fnk = Subst . unKA (S.categorySeqApply k)
    in shows (fmap (fnk . f) p)

    


round2 :: Subst a -> Subst a
round2 = Subst . back . fwd . unSubst



interpret :: forall a . Subst a -> Tree a
interpret (Subst (Return x)) = singleton x
interpret (Subst (Primitive (FMap f p :: RPair b) (k :: S.Seq (KleisliArrow (Program RPair)) b a))) = let
  fnk :: b -> Subst a
  fnk = Subst . unKA (S.categorySeqApply k)

  p' :: Pair (Tree a)
  p' = fmap (interpret . fnk . f) p

  in
   fromPairTree p'

leaf :: a -> Subst a
leaf = Subst . return

branch :: Subst Bool
branch = Subst (Primitive (FMap id (Pair (True, False))) S.empty)


tree2 :: Subst String
tree2 =
  ((leaf "a" <+++> leaf "r") <+++> leaf "x")
  <+++>
  (leaf "y" <+++> leaf "z")
  where
    l <+++> r = do
      t <- branch
      if t
        then l
        else r
-- round2 tree2 == tree2

main = do
  _ <- runTestTT $ TestList [
    "Rountrip 1" ~: assertEqual "" (show $ round1 tree1) (show tree1)
    , "Roundtrip 2 " ~: assertEqual "" (show $ round2 tree2) (show $ interpret tree2)
    ]
  return ()

