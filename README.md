# purescript-infinite-lists

*Getting back unsafe head's type, safely*

This library defines a type for strictly infinite (but lazy) lists.

As such, some of the `Data.List` API is not implemented here -- anything that
relies on finiteness, for example -- while other functions get simplified types.

For example:
```purescript
head :: forall a. List a -> a
tail :: forall a. List a -> List a
```
are now totally safe.

# Using

`Data.List.Infinite` is meant to be imported qualified, as almost all its exposed
functions clash with other modules.

Note also that the `Semigroup` instance provided is not an extension of the
finite version.

# installing

`bower i --save purescript-infinite-lists`

# other

It's amusing that, had they been provided, `length` could be given in O(1) time
instead of O(n), and `last` could be given a simple definition, retaining the
same type signature: `last _ = Nothing`.
