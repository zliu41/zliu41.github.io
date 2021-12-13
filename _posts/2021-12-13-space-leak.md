---
title: Understanding Space Leaks From StateT
date: 2021-12-13
---

There are two versions of `StateT` in the `transformers` package: lazy and strict. And each
version comes with two functions for updating
the state: the lazy `modify` and the strict `modify'`. As long as, one might contemplate, I
stick to the strict `StateT` and the strict `modify'`, it should be safe and I'd
need to worry no more about space leaks, right?

Wrong.

Worse, sometimes the space leak can be "hidden" by `-O` or `-O2`, or other GHC flags. It can
resurface when a seemingly unrelated change is made. The situation can also vary depending
on the underlying monad. Space leak is tricky business for Haskell developers
and this is just one example.

In this blog post I'll explain using a small program how space leaks can still occur
when using the strict `StateT` and the strict `modify'`. Also discussed include other
factors (e.g., GHC flags) that may impact memory consumption, things to look out for,
as well as stack space leaks vs. heap space leaks. I used GHC 9.2.1 to compile and run
the example program.

# A Simple Space Leak Example

```haskell
module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable

main :: IO ()
main = print $
  flip execState (Just 0 :: Maybe Int) $
    for_ [1 .. 100000] $ \i ->
      modify' $ fmap (+ i)
```

This program uses the strict `StateT` and the strict `modify'`, but it leaks space. Compiling it with `-rtsopts`,
then running it with `+RTS -K1M -RTS` (i.e., limiting the stack size to 1M), leads to stack overflow:

```
Main: Stack space overflow: current size 33624 bytes.
Main: Use `+RTS -Ksize -RTS' to increase it.
```

# Why the Strict StateT Does Not Prevent the Space Leak

First of all, not only does the strict `StateT` not always prevent space leaks, but it can
_cause_ space leaks. The relationship between the two versions of `StateT`
is similar to that between `foldl'` and `foldr`. Recall that as a general rule,
`foldl'` is the right choice when you are folding a structure into a single strict value,
while `foldr` is more appropriate if the result is a lazy and potentially infinite data
structure. In the latter case, the recursion in `foldr` is usually hidden behind a data
constructor. This is known as "guarded recursion", which ensures that it only computes
the part of the result that is needed. For example, `sum` should use `foldl'` and `map`
should use `foldr`. Using `foldr` for `sum` leaks memory, as does using `foldl'` for `map`.

Similarly, the strict `StateT` should be used if the result is a strict value, and the lazy
`StateT` if the result is a lazy structure. The difference between the two
versions of `StateT` is that the lazy version uses lazy pattern matching on intermediate
results (which are (value, state) pairs), essentially delaying the recursive call, and potentially
turning a non-guarded recursion into a guarded recursion. Just like `foldl'` and `foldr`, using the
wrong version of `StateT` could potentially lead to memory leaks.

That being said, unlike `foldl'` and `foldr`, the situation with `StateT` also depends
on the underlying monad. In general, for underlying monads
whose `(>>=)` operator is lazy, such as `Identity`[^1], `Reader`, and the lazy `Writer`, choosing
the right version of `StateT` is important. On the other hand, underlying monads with strict `(>>=)`
operators, like `IO`, `Maybe`, the strict `Writer` and `[]`, render the choice of lazy vs. strict `StateT`
moot, since the `(>>=)` of `StateT` would be strict anyway even for the lazy `StateT`.[^2]

Here are some examples where the underlying monad is `Identity`, and choosing the proper version of
`StateT` _does_ matter:

```haskell
-- Must use lazy StateT: the result is a lazy structure, [Int].
main :: IO ()
main = print $ take 5 $ evalState (go 0) ()
  where
    go :: Int -> State () [Int]
    go x
      | x == 100000 = pure []
      | otherwise = do
        rest <- go (x + 1)
        pure (x : rest)

-- Must use strict StateT
main :: IO ()
main = print $ runState (go 0) 0
  where
    go :: Int -> State Int ()
    go x
      | x == 100000 = pure ()
      | otherwise = do
        modify' (+ 1)
        go (x + 1)
```

Now, back to the question of why the strict `StateT` doesn't prevent the space leak
in our original example. In this case the underlying monad is `Identity`, and we
don't need the result, so the strict `StateT` is the right choice. However, it is
only strict in the sense that the intermediate (value, state) pairs are evaluated to
weak head normal form (WHNF). Since `(,)` is lazy, this does not force either the value
or the state. At issue here in our example is that the state keeps getting bigger
and bigger without being evaluated, and the strict `StateT` can't prevent it from
happening.

# Why the Strict modify' Does Not Prevent the Space Leak

`modify'` does force the new state, but only to WHNF. If the state type in our example
was `Int`, then we'd have no problem. But it's `Maybe Int`, which means `modify'` will
only evaluate it to `Just <thunk>`, but does not evaluate the thunk to WHNF. Then
what winds up happening is that the thunk under the `Just` keeps growing,
and is only collapsed at the end.

# Why O2 Does Prevent (This Particular) Space Leak

When compiling our example with `-O2`, the space leak goes away, and running it with `-K1M`
successfully prints "Just 5000050000". How come? It turns out this is the effect of two
flags implied by `-O2`:

- `-fno-ignore-interface-pragmas` (i.e., `-O2` turns off `-fignore-interface-pragmas`). Ignoring
  interface pragmas means nothing will be inlined. By not ignoring interface pragmas, the `+`
  operator can be inlined, which makes it possible for `-fspec-constr` to generate more efficient code.
- `-fspec-constr`. This flag turns on call-pattern specialization, which is described in paper
  [_Call-pattern specialisation for Haskell programs_](https://www.microsoft.com/en-us/research/publication/system-f-with-type-equality-coercions-2/).
  Section 2.1 of the paper uses `drop` as an example to show how call-pattern specialization
  creates a specialized function `drop' :: Int# -> [a] -> [a]` (vs. `drop: Int -> [a] -> [a]`).
  `drop'`'s first argument is an unboxed type, hence strict. It is exactly for this reason that compiling
  with `-O2` produces, in this case, more strict code.[^3]

Compiling with `-O` itself does not prevent the space leak, because `-O` does not imply `-fspec-constr`.
`-O -fspec-constr` does the job.

Compiling with `-O0 -fno-ignore-interface-pragmas -fspec-constr`, however, cannot prevent the
space leak, because `-O0` ignores a number of optimization flags, including `-fspec-constr`.

Although `-O2` prevents space leaks in this particular case, it is not something you can rely on to fix space
leaks in general. It isn't difficult at all to produce a space leak with `-O2` enabled. For example, if you
do either one of these two things, then `-O2` is no longer capable of preventing the space leak:

- Change `fmap (+ i)` to `fmap (+ if even i then 1 else 2)`
- Or, create a wrapper for `+` marked `NOINLINE`, then use the wrapper in place of `+`

This shows when compiling with optimizations, space leaks can occur after making a seemingly innocent
and unrelated change, which can further add to the confusion, and that's not a good situation to be in.

# Pay Attention to Whether You Are Forcing the Right Things

A few common ways of making things more strict include `seq`, `deepseq` and bang patterns. But
before rushing to reach for them,
you may want to make sure you are going to use them correctly.
You may be tempted, for example, to do something like this:

```
- modify' $ fmap (+ i)
+ modify' $ fmap $ \(!x) -> Control.DeepSeq.force (x + i)
```

or

```
- modify' $ fmap (+ i)
+ modify' $ \(!x) -> fmap (+ i) x
```

Does either of these fix the space leak? Nope. In the first attempt, we are forcing
things _within_ `fmap`, and that doesn't help. Recall that
`fmap f (Just a) = Just (f a)`. And since `modify'` doesn't evaluate things beyond `Just`,
the `f a` won't be evaluated till the end.
In the second attempt, the `(!x)` only evaluates `x :: Just Int` to WHNF, which is
useless: `modify'` already evaluates it to WHNF, and we need to evaluate it beyond WHNF.

The proper fix is to ensure the new state is fully evaluated whenever the argument to
`modify'` is evaluated to WHNF. Two possible fixes are:

```
- modify' $ fmap (+ i)
+ modify' $ \case Just (!j) -> Just (i + j); Nothing -> Nothing
```

and

```
- modify' $ fmap (+ i)
+ modify' $ Control.DeepSeq.force . fmap (+i)
```

# Stack vs. Heap

What if we only need to evaluate the final state to WHNF (and not NF)? Suppose we make the
following change to the original example:

```
- main = print $
+ main = print $ isJust $
```

Now we only need to know whether the final state is `Just` or `Nothing`, but don't need the exact
value. Indeed, if we compile it with `-O0` and run it with `-K1M`, it would successfully print
out `Just`. So we are all good, right?

Wrong. Now it doesn't leak space on the stack, but it still leaks space on the heap. If we run it
with `-M2M` (which limits the maximum heap space to 2M), then we'd get

```
Main: Heap exhausted;
Main: Current maximum heap size is 2097152 bytes (2 MB).
Main: Use `+RTS -M<size>' to increase it.
```

Remember, thunks are stored on the heap. The stack is consumed when a thunk is evaluated. In this
case we don't need a lot of stack space since we don't need to evaluate the thunk beyond
`Just`, but the thunk still occupies the heap.

By the way, in case you come from another programming languages and are relatively new to Haskell, and
don't know this: the stack in Haskell is _not_ the call stack employed in the runtime of most
other programming languages. Haskell as a lazy language has an entirely different and fairly
unique execution model which doesn't use call stacks to run subroutines. Its "stack" is part of
the spineless tagless G-machine, an
abstract machine for evaluating lazy functional code. In Haskell there's no such thing as
"one stack per thread", and the stack size can be much larger than the typical size of a
call stack. Haskell's default stack size is 80% of the heap size.

# StrictData

Finally, though not applicable to our particular example, it bears noting that
`StrictData` is a good extension to use by default. It tends to make the code much more robust
with respect to space leaks caused by unevaluated expressions.

---

[^1]: `Identity`'s `(>>=)` operator is lazy because `Identity` is a newtype wrapper; it doesn't
      exist at runtime, and so pattern matching on `Identity` doesn't force evaluation (in
      other words, `case undefined of Identity _ -> 3` evaluates to 3). If `Identity` was
      defined using data, then its `(>>=)` would be strict.

[^2]: The list monad (`[]`) as an underlying monad is especially prone to space leaks,
      because its `(>>=)` operator desugars to `concatMap`, which essentially delays pattern
      matching on `[]`'s data constructors till the end, even for the strict `StateT`. If your
      use case demands using list as either the underlying monad, or the monad transformer itself ("`ListT`"),
      consider using a streaming library like `conduit` or `pipes`.

[^3]: You may go and check the outputs from `-ddump-simpl`, both with and without `-fspec-constr`,
      and see if you can spot the effect of `-fspec-constr`.
