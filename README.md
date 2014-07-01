typed-seq
=========

[![Build Status](https://travis-ci.org/lambdageek/typed-seq.svg)](https://travis-ci.org/lambdageek/typed-seq)

This is an implementation of the ideas from
[Reflection Without Remorse](http://homepages.cwi.nl/~ploeg/papers/zseq.pdf)
by [van der Ploeg](http://homepages.cwi.nl/~ploeg/) and [Kiselyov](http://okmij.org/ftp/)

The included `Data.TypedSequence` implements a type-aligned sequence.
That is, something like
[`Data.Sequence`](http://hackage.haskell.org/package/containers/docs/Data-Sequence.html),
but with each element of kind `* -> * -> *` such that the
second type index of an element is equal to the first type index of its successor.

Also included are
1. an implementation of a `Free f` monad of a functor `f` in terms of type-directed sequences;
2. a `Program i` monad in the style of the 
[`operational`](http://hackage.haskell.org/package/operational) package
builds a monad out of any GADT `i` describing the primitive operations.
3. A pair of terms that witness a bijection `Free f` ≅ `Program (ReifiedFunctor f)` where `ReifiedFunctor f` is
 a primitive operation that denotes the `fmap` operation of the functor `f`.

