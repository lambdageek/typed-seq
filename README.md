typed-seq
=========

This is an implementation of the ideas from "Reflection Without Remorse" by van der Ploeg and Kiselyov.

The included `Data.TypedSequence` implements a type-directed sequence.
That is, something like `Data.Sequence`, but with each element of kind
`* -> * -> *` such that the corresponding first and second type
indices of each element match up.

Also included is an implementation of a free monad in terms of type-directed sequences.

Also included is a monad ala `operational` that builds a monad out of any GADT describing the primitive operations.

Also included are terms that witness a bijection between the
operational monad and the free monad over a suitable functor.

