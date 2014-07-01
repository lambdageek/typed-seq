{-#
  LANGUAGE GADTs, RankNTypes, ScopedTypeVariables
  #-}
module Control.Monad.FreeReflect (
  -- * Free monad construction on a functor
  Free
  , FreeView(..)
  , toView
  , fromView
    -- * An alternative construction
  , Program(..)
    -- * Bijection witnesses
  , ReifiedFunctor(..)
  , fwd
  , back
    -- * Utility - Klesli Arrows
  , KleisliArrow(..)
  , knat
  ) where

import qualified Control.Category as Cat
import Control.Monad

import qualified Data.TypedSequence as S

newtype KleisliArrow m a b = KA { unKA :: a -> m b}

instance Monad m => Cat.Category (KleisliArrow m) where
  id = KA return
  (KA f2) . (KA f1) = KA (f1 >=> f2)

type Natural f g = forall a . f a -> g a

knat :: Natural m n -> S.Natural2 (KleisliArrow m) (KleisliArrow n)
-- (forall a . m a -> n a) -> (forall a b . (a -> m b) -> (a -> n b))
knat p (KA mf) = KA (\x -> p (mf x))


data Free f a where
  Free :: FreeView f a -> S.Seq (KleisliArrow (Free f)) a b -> Free f b

data FreeView f a = Pure a | Step (f (Free f a))

fromView :: FreeView f a -> Free f a
fromView x = Free x S.empty

toView :: Functor f => Free f a -> FreeView f a
toView (Free (Pure x) s) = case S.viewL s of
  S.NilL -> Pure x
  (KA f S.:< s') -> toView (f x >>>=  s')
toView (Free (Step fx) s) = Step (fmap (\x -> x >>>= s) fx)

(>>>=) :: Free f a -> (S.Seq (KleisliArrow (Free f)) a b) -> Free f b
(Free x s1) >>>= s2 = Free x (s1 S.>< s2)

instance Functor f => Functor (Free f) where
  fmap = liftM

instance Functor f => Monad (Free f) where
  return x = fromView (Pure x)
  (Free h s) >>= mf = Free h (s `S.snoc` KA mf)

data Program instr a where
  Return :: a -> Program instr a
  Primitive :: instr b -> S.Seq (KleisliArrow (Program instr)) b a -> Program instr a

data ReifiedFunctor f a where
  FMap :: (a -> b) -> f a -> ReifiedFunctor f b

instance Monad (Program instr) where
  return = Return
  (Return x) >>= f = f x
  (Primitive p s1) >>= f = Primitive p (s1 `S.snoc` KA f)

fwd :: forall f a . Functor f => Program (ReifiedFunctor f) a -> Free f a
fwd (Return x) = return x
fwd (Primitive (FMap (f :: b' -> c) (r :: f b')
                -- :: ReifiedFunctor f c
               )
               (ops :: S.Seq (KleisliArrow (Program (ReifiedFunctor f))) c a)) =
  Free (Step $ fmap (return . f) r) (fwdOps ops)

absorb :: Functor f => f (Program (ReifiedFunctor f) a) -> Program (ReifiedFunctor f) a
absorb fp = Primitive (FMap id fp) (S.singleton $ KA id)

back  :: forall f a . Functor f => Free f a -> Program (ReifiedFunctor f) a
back m = case toView m of
   Pure x -> Return x
   Step (f :: f (Free f a)) ->
     absorb (fmap back f) -- f (Program (RF f) a)

fwdOps :: Functor f => S.Natural2 (S.Seq (KleisliArrow (Program (ReifiedFunctor f)))) (S.Seq (KleisliArrow (Free f)))
fwdOps = S.nat2 (knat fwd)

-- -- backOps :: Functor f => S.Natural2 (S.Seq (KleisliArrow (Free f))) (S.Seq (KleisliArrow (Program (ReifiedFunctor f))))
-- -- backOps = S.nat2 (knat back)

