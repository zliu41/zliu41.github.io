---
title: Data.List.genericLength and Space Leaks
date: 2019-12-05
---

The codebase I regularly work with uses in many places integer types that are not `Int`, including
`Integer`, `Natural`, `Word32`, `Word64`, etc. A number of functions in [`Data.List`](http://hackage.haskell.org/package/base/docs/Data-List.html),
however, only works with `Int`, such as `length`, and so there's a lot of `fromIntegral`s floating around,
which isn't pretty. I was once tempted to get rid of all these `fromIntegral`s by using `genericLength`, and
a coworker reviewing the PR pointed out that the doc of `genericLength` mentions the following:

> It is, however, less efficient than length.

My first reaction was, how can `genericLength` be slower than `fromIntegral . length`? If that is the case, why wouldn't it
be implemented as `fromIntegral . length`?

So I took a look at the [actual implementation](http://hackage.haskell.org/package/base/docs/src/Data.OldList.html#genericLength):

```haskell
genericLength           :: (Num i) => [a] -> i
{-# NOINLINE [1] genericLength #-}
genericLength []        =  0
genericLength (_:l)     =  1 + genericLength l

{-# RULES
  "genericLengthInt"     genericLength = (strictGenericLength :: [a] -> Int);
  "genericLengthInteger" genericLength = (strictGenericLength :: [a] -> Integer);
 #-}

strictGenericLength     :: (Num i) => [b] -> i
strictGenericLength l   =  gl l 0
                        where
                           gl [] a     = a
                           gl (_:xs) a = let a' = a + 1 in a' `seq` gl xs a'
```

It is apparent from the above that `genericLength` is basically implemented as a right fold, except
that when `i` is either `Int` or `Integer`, it uses `strictGenericLength` instead, which is
essentially a strict left fold. The default right fold implementation can be extremely
inefficient because it builds up a thunk,

```
1 + (1 + (1 + ...
```

that is as large as the list itself, before starting to reduce it. For a detailed explanation
on why `foldr` (and the lazy `foldl`) can cause space leaks, and why the strict `foldl'` is a much better
option for such cases, see "[foldr foldl foldl'](https://wiki.haskell.org/Foldr_Foldl_Foldl')". What this
means is that `genericLength` is as efficient as length for `Int` and `Integer`, but much less efficient
for many other integer types like `Word` and `Natural`.

This begs the question: why is `genericLength` implemented this way? Why not always use `strictGenericLength`?
I asked this very question on [Stack Overflow](https://stackoverflow.com/questions/56157700/why-is-data-list-genericlength-implemented-as-a-right-fold),
and the answer is that it is implemented this way so that it can work with lazy natural numbers. This is also mentioned
in [this GHC wiki page](https://gitlab.haskell.org/ghc/ghc/wikis/prelude710/ftp#similarly-things-like-length-could-be-generalized-to-num-making-length-and-genericlength-equivalent).
The wiki page also mentions another argument against `genericLength`: code that uses it and does not give
the result an explicit signature will get `Integer` by default, which is usually not what one wants. In this case,
though, GHC will give you a "type defaults" warning, so as long as you enable all warnings, you shouldn't
get an `Integer` unknowingly.

The other generic functions in `Data.List`, including `genericTake`, `genericDrop`, `genericSplitAt`, `genericIndex`
and `genericReplicate`, are safe to use as they don't have the same issue as `genericLength`. To
sum up, it is a good idea to almost always avoid `genericLength` unless you really know what you are doing.
