{-#
  LANGUAGE
    GADTs
  , RankNTypes
  , ScopedTypeVariables
  #-}
module Data.TypedSequence (
  -- * Catenable type-directed sequences
  Seq,
  -- * views
  ViewL (..),
  ViewR (..),
  viewL,
  viewR,
  -- * construction
  empty,
  singleton,
  cons,
  snoc,
  (><),
  -- * Conversion
  Const2,
  toList,
  Natural2,
  nat2,
  foldl,
  foldr,
  categorySeqApply,
  categorySeqApply'
  ) where

import Prelude hiding (foldl, foldr)
import qualified Control.Category as Cat
import Control.Category (Category)

data ViewL arr a b where
  NilL :: ViewL arr a a
  (:<) :: arr a b -> Seq arr b c -> ViewL arr a c

data ViewR arr a b where
  NilR :: ViewR arr c c
  (:>) :: Seq arr a b -> arr b c -> ViewR arr a c

data Seq arr a b where
  Empty :: Seq arr a a
  One :: arr a b -> Seq arr a b
  Several :: Bin arr a b -> Seq (Tup arr) b c -> Bin arr c d -> Seq arr a d

data Tup arr a b where
  Tup2 :: arr a b -> arr b c -> Tup arr a c
  Tup3 :: arr a b -> arr b c -> arr c d -> Tup arr a d

data Bin arr a b where
  Bin1 :: arr a b -> Bin arr a b
  Bin23 :: Tup arr a b -> Bin arr a b

empty :: Seq arr a a
empty = Empty

singleton :: arr a b -> Seq arr a b
singleton = One

infixr 6 `cons`

cons :: arr a b -> Seq arr b c -> Seq arr a c
cons x Empty = One x
cons x (One y) = Several (Bin1 x) Empty (Bin1 y)
cons x (Several (Bin1 y) m r) = Several (Bin23 $ Tup2 x y) m r
cons x (Several (Bin23 p) m r) = Several (Bin1 x) (cons p m) r

infixl 6 `snoc`

snoc :: Seq arr a b -> arr b c -> Seq arr a c
snoc Empty x = One x
snoc (One x) y = Several (Bin1 x) Empty (Bin1 y)
snoc (Several l m (Bin1 x)) y = Several l m (Bin23 $ Tup2 x y)
snoc (Several l m (Bin23 p)) y = Several l (m `snoc` p) (Bin1 y)

viewL :: Seq arr a b -> ViewL arr a b
viewL Empty = NilL
viewL (One x) = x :< Empty
viewL (Several (Bin23 (Tup2 x y)) m r) = x :< Several (Bin1 y) m r
viewL (Several (Bin23 (Tup3 x y z)) m r) = x :< Several (Bin23 (Tup2 y z)) m r
viewL (Several (Bin1 x) m r) = x :< pullL m r

pullL :: Seq (Tup arr) a b -> Bin arr b c -> Seq arr a c
pullL m r = case viewL m of
  NilL -> binToSeq r
  (l :< m') -> Several (Bin23 l) m' r

binToSeq :: Bin arr a b -> Seq arr a b
binToSeq (Bin1 x) = One x
binToSeq (Bin23 (Tup2 x y)) = Several (Bin1 x) Empty (Bin1 y)
binToSeq (Bin23 (Tup3 x y z)) = Several (Bin1 x) Empty (Bin23 (Tup2 y z))

viewR :: Seq arr a b -> ViewR arr a b
viewR Empty = NilR
viewR (One x) = Empty :> x
viewR (Several l m (Bin23 (Tup2 y z))) = Several l m (Bin1 y) :> z
viewR (Several l m (Bin23 (Tup3 x y z))) = Several l m (Bin23 (Tup2 x y)) :> z
viewR (Several l m (Bin1 z)) = pullR l m :> z

pullR :: Bin arr a b -> Seq (Tup arr) b c -> Seq arr a c
pullR l m = case viewR m of
  NilR -> binToSeq l
  (m' :> r) -> Several l m' (Bin23 r)

infixr 5 ><

(><) :: Seq arr a b -> Seq arr b c -> Seq arr a c
Empty >< s = s
s >< Empty = s
(One x) >< s = x `cons` s
s >< (One x) = s `snoc` x
(Several l m1 b1) >< (Several b2 m2 r) = Several l (appendBin m1 b1 b2 m2) r


appendBin :: Seq (Tup arr) a b -> Bin arr b c -> Bin arr c d -> Seq (Tup arr) d e -> Seq (Tup arr) a e
appendBin s1 (Bin1 x) (Bin1 y) s2 = s1 >< (Tup2 x y) `cons` s2
appendBin s1 (Bin1 x) (Bin23 (Tup2 y z)) s2 = s1 >< (Tup3 x y z) `cons` s2
appendBin s1 (Bin1 x) (Bin23 (Tup3 y z w)) s2 = s1 `snoc` (Tup2 x y) >< (Tup2 z w) `cons` s2
appendBin s1 (Bin23 (Tup2 x y)) (Bin1 z) s2 = s1 >< (Tup3 x y z) `cons` s2
appendBin s1 (Bin23 (Tup3 x y z)) (Bin1 w) s2 = s1 `snoc` (Tup2 x y) >< (Tup2 z w) `cons` s2
appendBin s1 (Bin23 t1) (Bin23 t2) s2 = s1 `snoc` t1 >< t2 `cons` s2

-- < Arrow

-- | Compose arrows in the sequence, associating from the left.
foldl :: Category arr => Seq arr a b -> arr a b
foldl s =
  case viewL s of
    NilL -> Cat.id
    (f :< fs) -> foldl fs Cat.. f

-- | Compose arrows in the sequence, associating from the right.
foldr :: Category arr => Seq arr a b -> arr a b
foldr s =
  case viewR s of
    NilR -> Cat.id
    (fs :> f) -> f Cat.. categorySeqApply' fs

{-# DEPRECATED categorySeqApply, categorySeqApply' "Use foldl and foldr instead" #-}

categorySeqApply :: Category arr => Seq arr a b -> arr a b
categorySeqApply = foldl

categorySeqApply' :: Category arr => Seq arr a b -> arr a b
categorySeqApply' = foldr


data Const2 t a b = Const2 { getConst2 :: t }
                    deriving Show


type Natural2 f g = forall a b . f a b -> g a b

nat2 :: forall f g . Natural2 f g -> Natural2 (Seq f) (Seq g)
nat2 p = go
  where
    go :: Natural2 (Seq f) (Seq g)
    go s = case viewL s of
      NilL -> empty
      (x :< s') -> p x `cons` go s'

toList :: Seq (Const2 t) a b -> [t]
toList s = case viewL s of
  NilL -> []
  (x :< s') -> getConst2 x : toList s'
  
