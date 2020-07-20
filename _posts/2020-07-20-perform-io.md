---
title: How Accursed and Unutterable is accursedUnutterablePerformIO?
date: 2020-07-20
---

Side effects break referential transparency. If you have a function `f` which has a pure type but performs
side effects behind closed doors (e.g., `f x = unsafePerformIO ...`), you'd have to be careful when refactoring the
code. You may want to, for instance, avoid changing `f x + f x` into `let y = f x in y + y`.

But even if you never make such substitutions yourself, the compiler may decide to step in and
do it for you. This post summarizes circumstances in which GHC may perform
optimizations that could change the behavior of your program, in the presence of non-referentially transparent
functions like `unsafePerformIO`, `unsafeDupablePerformIO`, and `accursedUnutterablePerformIO`. Hopefully this
helps you understand how to safely use them if you absolutely must do so.

By the way, the reason the title specifically features `accursedUnutterablePerformIO` is twofold:
1. `accursedUnutterablePerformIO` is the least safe of the three. Therefore, most if not all what
    I'm about to say that applies to `unsafePerformIO` and `unsafeDupablePerformIO` is also applicable to `accursedUnutterablePerformIO`.
2. `accursedUnutterablePerformIO` is such a priceless name and I just couldn't help myself.

# Preliminaries

Here's some useful background before we dive into the details. Some of the things GHC can do that alter the behaviors of
side-effectful functions include:

- Common subexpression elimination (CSE). CSE rewrites `f x + f x` into `let y = f x in y + y`, which may decrease
  the number of times side effects are performed. CSE is turned on by default when compiling with `-O` and `-O2`, and can be
  disabled with the `-fno-cse` flag.
- Full laziness optimization. The definition of full laziness is "every expression is evaluated at most once after the variables in
  it have been bound". This is achieved by floating bindings out of lambdas, for example, `f x = let y = g 42 in x + y`
  is rewritten into `y = g 42; f x = x + y`. By floating `y = g 42` out of `f`, `g 42` is evaluated
  at most once, which may also decrease the number of times side effects are performed. The original definition, on the other
  hand, would evaluate `g 42` every time `f` is applied to an `x`. Full laziness is also turned on when compiling with `-O` and `-O2`, and can be
  disabled with the `-fno-full-laziness` flag.
- Inlining. This has the opposite outcome with respect to side effects: inlining a side-effectful definition may cause
  the side effect to be performed more times compared to not inlining it.

It is worth mentioning that these three things don't act independently, but are related and sometimes interact with one another
in ways that can be really subtle, as the examples that follow shall demonstrate.

About the three unsafe functions covered in this post:
- `unsafePerformIO` is the most well-known, and is relatively the safest, but the slowest of the three.
- `unsafeDupablePerformIO` is faster but also less safe, in the sense that it provides
  no guarantee that an `IO` action won't be performed simultaneously by multiple threads.
- `accursedUnutterablePerformIO` (from the `bytestring` package) is even faster but much less safe. Not
  only does it not check for duplicates, but it inlines everything, which makes CSE a lot more likely to
  be triggered. It used to be called `inlinePerformIO`, but `accursedUnutterablePerformIO` is an undeniably more
  advisable name.

All of the following examples assume GHC 8.10.1 and `-O2`.

# The Effect of CSE

Let's start with something simple:

```haskell
module Main where
import System.IO.Unsafe

hasSideEffect :: Int -> Int
hasSideEffect x = unsafePerformIO $ do
  putStrLn "side effect!"
  pure x

main :: IO ()
main = print (hasSideEffect 0 + hasSideEffect 0)
```

`hasSideEffect 0` is obviously a common subexpression in the body of `main`. It would be factored out if CSE
is turned on, meaning "side effect!" would only be printed once. Passing `-fno-cse` to GHC makes it print twice.

Note that it doesn't matter if `hasSideEffect` is inlined or not. `hasSideEffect 0` is a common subexpression
in either case.

# The Effect of Full Laziness

The following example demonstrates the effect of the full laziness optimization:

```haskell
module Main where
import System.IO.Unsafe

hasSideEffect :: Int -> Int
hasSideEffect x = unsafePerformIO $ do
  putStrLn "side effect!"
  pure x

f :: Int -> Int
f x =
  let g y = let n = hasSideEffect x
             in if y == 0 then 0 else g (y-1) + n
   in g 5

main :: IO ()
main = print (f 0)
```

The full laziness optimization causes "side effect!" to be only printed once. Although the value of
`n = hasSideEffect x` is used five times when evaluating `g 5`, `n` is only evaluated once, because
it is floated out of the body of `g`. If we disable full-laziness by passing `-fno-full-laziness`, you'd
see "side effect!" printed five times.

In this example there's no common subexpression to eliminate, so disabling CSE has no effect.

# The Effect of Inlining

Inlining, on the contrary, can increase the number of times a side effect is performed.

```haskell
module Main where
import System.IO.Unsafe

hasSideEffect :: Int
hasSideEffect = unsafePerformIO $ do
  putStrLn "side effect!"
  pure 42
{-# NOINLINE hasSideEffect #-}

x :: Int
x = hasSideEffect + 1

y :: Int
y = hasSideEffect + 2

main :: IO ()
main = print (x + y)
```

This program prints "side effect!" once, which is usually what you want for having a constant value defined using `unsafePerformIO`.
Turning off CSE and/or full laziness does not change the behavior. If you change `{-# NOINLINE hasSideEffect #-}` to
`{-# INLINE hasSideEffect #-}`, then "side effect!" would be printed twice: once when evaluating `x` and once for `y`.

When there is neither an `INLINE` nor a `NOINLINE` pragma, GHC doesn't seem to like inlining constants. It is, however, free
to do so. For constants that contain side effects, such as `hasSideEffect`, `x` and `y` in the above example, it is
therefore almost always advisable to apply the `NOINLINE` pragma. In fact, HLint warns
against missing `NOINLINE` pragmas for constants defined using `unsafePerformIO`s. This advice, of course, isn't
applicable to functions with `unsafePerformIO`s in their bodies. For a function like `f x = unsafePerformIO (... x ...)`, the
side effect needs to be performed whenever `f` is applied to an `x`, so it may as well be inlined.

# Interaction between Inlining, CSE and Full Laziness

Consider this program.

```haskell
module Main where
import System.IO.Unsafe

hasSideEffect :: Int -> Int
hasSideEffect x = unsafePerformIO $ do
  putStrLn "side effect!"
  pure x

f :: Int -> Int
f x =
  let g y = let z = hasSideEffect x in y + z
      {-# INLINE g #-}
   in g 0 + g 1

main :: IO ()
main = print (f 0)
```

This program prints "side effect!" just once, whether `g` is inlined or not inlined. In order to make "side effect!"
print twice, you need to disable CSE if `g` is inlined, but disable
full laziness if `g` is not inlined. Let's study why this is the case.

If `g` is inlined, `g 0 + g 1` becomes

```haskell
(let z = hasSideEffect x in 0 + z) + (let z = hasSideEffect x in 1 + z)
```

`hasSideEffect x` is a common subexpression, so it is factored out by CSE, causing "side effect!" to be printed
just once. In this case the inlining happened in a simplifier pass before the full laziness pass (which can be
observed from the output of `-dshow-passes`), and therefore, the full laziness pass didn't have the opportunity
to float `z = hasSideEffect x` out of `g` before `g` was inlined. This is why turning off
full laziness has no effect, but turning off CSE does.

If `g` is not inlined, on the other hand, there would be no common subexpression to eliminate.
Instead, the full laziness pass would float `z = hasSideEffect x` out of `g`. This is why turning off
CSE has no effect, but turning off full laziness does.

It is also possible to make it so that _both_ `-fno-cse` and `-fno-full-laziness` are needed in order for
"side effect!" to be printed twice. For example, try `{-# NOINLINE [2] g #-}`. This is because
- If full-laziness is turned on, `z = hasSideEffect x` would be floated out of `g` _before_ inlining (because we told
  GHC to avoid inlining until after phase 2, which comes after the full laziness pass). After `z = hasSideEffect x`
  is floated out, it would _not_ subsequently be inlined, and therefore "side effect!" is only printed once.
- If CSE is turned on, it would happen _after_ inlining. Since inlining enables CSE to "see" the common
  subexpression `hasSideEffect x`, it is factored out, and therefore “side effect!” is only printed once.

Simply put, in this case inlining happens after the full-laziness pass but before the CSE pass, and this means
you need to turn them both off for the side effect to be performed twice.

This example shows that the interaction between CSE, full-laziness and inlining can be subtle. If your
program uses `unsafePerformIO` or similar unsafe functions, the simplest way to ensure that the behavior is easily predictable
is to either `NOINLINE` everything and turn off both CSE and full-laziness, or compile the program with `-O0`.
You don't really want to do either of them because they both kill performance, so it really pays to understand the nuances so that you lose as little performance as possible while ensuring the program behaves as expected.

# unsafeDupablePerformIO

Using `unsafeDupablePerformIO` instead of `unsafePerformIO` is another way to potentially make an IO action
perform more times than intended. This is slightly off topic since it is about concurrency rather than GHC modifying
your program, but since `unsafeDupablePerformIO` falls in between the relatively safe `unsafePerformIO` and the
extremely unsafe `accursedUnutterablePerformIO`, I reckon it is worth mentioning.

A thunk is usually evaluated only once. When a thread attempts to force a thunk, and that thunk is already being
evaluated by another thread, the first thread would usually block rather than evaluating the thunk again. This is
achieved using a technique called [blackholing](https://www.microsoft.com/en-us/research/wp-content/uploads/2005/09/2005-haskell.pdf).
Blackholing is by default lazy, but an eager option is possible via flag `-feager-blackholing`, which guarantees
that no thunk is evaluated more than once (with a slight performance penalty).

If a thunk is built using `unsafePerformIO`, then even if the thunk is evaluated by more than one thread
in parallel, the IO action is guaranteed to be performed only once. `unsafeDupablePerformIO`, on the other hand,
doesn't guarantee that. And when the same thunk is evaluated concurrently by two threads, one of them may be cancelled
without an exception being raised.

Unfortunately I haven't been able to find an example where `unsafePerformIO` and `unsafeDupablePerformIO`
behave differently. Based on what I read about this, it seems fairly easy to trigger multiple evaluations of
the same thunk in earlier GHC versions. However, it appears to be very difficult to do so in GHC 8.10.1. Still, you should as a
general rule only use `unsafeDupablePerformIO` if it is acceptable for the IO action to be (1) performed
multiple times and (2) cancelled without giving you a chance to clean up. For instance, it may be acceptable
for an IO action reading a memory location to be occasionally performed multiple times, provided the
data being read doesn't change. On the other hand, opening a database connection and querying
the database is probably not a good idea.

# accursedUnutterablePerformIO

Now finally, `accursedUnutterablePerformIO` gets up on stage. A previous example  already shows that inlining a
function can potentially trigger CSE. Here's another example that shows this more clearly:

```haskell
module Main where
import System.IO.Unsafe

f :: Int -> Int
f x = x + unsafePerformIO (putStrLn "side effect!" >> pure 42)
{-# NOINLINE f #-}

main :: IO ()
main = print (f 1 + f 2)
```

Here `f` is not inlined, so there's no common subexpression to eliminate, and "side effect!" would be printed twice. If you
dare to inline `f`, however, then `unsafePerformIO (putStrLn "side effect!" >> pure 42)` would become a common subexpression
in `f 1 + f 2`, triggering CSE, causing "side effect!" to be printed only once.

This makes a lot of sense, and is quite easy to understand. Now, let's change the definition of `f` a little bit:

```haskell
import System.IO.Unsafe

f :: Int -> Int
f x = unsafePerformIO (putStr "side effect: " >> print x >> pure x)
{-# INLINE f #-}

main :: IO ()
main = print (f 1 + f 2)
```

This program would print

```
side effect: 1
side effect: 2
3
```

which, too, makes perfect sense. Although `f` is inlined which means there are two occurrences of
`putStr "side effect: "` in `f 1 + f 2`, factoring it out should make no difference, because it is an `IO`
action, and `IO` actions (as opposed to supposedly pure values performing side effects under the hood) are
referentially transparent. That is to say, even if CSE rewrites `f 1 + f 2` into

```haskell
let common = putStr "side effect: "
 in unsafePerformIO (common >> print 1 >> pure 1) + unsafePerformIO (common >> print 2 >> pure 2)
```

"side effect: " should still be printed twice. This is also the case if `f` is defined in terms
of `unsafeDupablePerformIO`.

But what if `f` is defined in terms of `accursedUnutterablePerformIO`? Try it, and you'd find the output to be

```haskell
side effect: 1
2
3
```

So apparently, CSE not only factored `putStr "side effect: "` out, but somehow had it performed only once! How is this possible?

To understand what's going on, let's compare the definitions of `accursedUnutterablePerformIO` and `unsafeDupablePerformIO`.

```haskell
{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, a #) -> a

{-# INLINE unsafeDupablePerformIO #-}
unsafeDupablePerformIO  :: IO a -> a
unsafeDupablePerformIO (IO m) = case runRW# m of (# _, a #) -> a

{-# NOINLINE runRW# #-}
runRW# :: forall (r :: RuntimeRep) (o :: TYPE r). (State# RealWorld -> o) -> o
runRW# m = m realWorld#

newtype IO a = IO { unIO :: State# RealWorld -> (# State# RealWorld, a #) }
```

So `accursedUnutterablePerformIO` is simply `unsafeDupablePerformIO` with `runRW# m` inlined. To see what this
implies, let's define

```haskell
m1 = unIO (putStr "side effect!")
m2 x = unIO (print x)
m3 x = unIO (pure x)
```

Then, if `f` is defined using `accursedUnutterablePerformIO`, `f 1 + f 2` would be expanded into

```haskell
f 1 + f 2
= let (# realWorld1, a1 #) = m1 realWorld#
      (# realWorld2, a2 #) = m2 1 realWorld1
      (# realWorld3, a3 #) = m3 1 realWorld2
   in a3
  +
  let (# realWorld1', a1' #) = m1 realWorld#
      (# realWorld2', a2' #) = m2 2 realWorld1'
      (# realWorld3', a3' #) = m3 2 realWorld2'
   in a3'
```

Without CSE this would print "side effect!" twice, but with CSE it would be turned into

```haskell
let common = m1 realWorld#
 in (let (# realWorld1, a1 #) = common
         (# realWorld2, a2 #) = m2 1 realWorld1
         (# realWorld3, a3 #) = m3 1 realWorld2
      in a3
     +
     let (# realWorld1', a1' #) = common
         (# realWorld2', a2' #) = m2 2 realWorld1'
         (# realWorld3', a3' #) = m3 2 realWorld2'
      in a3'
    )
```

Therefore the effect in `m1 realWorld#` is only performed once, which means "side effect: " is
only printed once.

On the other hand, if `f` is defined using `unsafeDupablePerformIO`, which avoids inlining
`runRW#`, then `f 1 + f 2` would be expanded into

```
case runRW# (\realWorld -> let (# realWorld1, a1 #) = m1 realWorld
                               (# realWorld2, a2 #) = m2 1 realWorld1
                               (# realWorld3, a3 #) = m3 1 realWorld2
                            in (# realWorld3, a3 #)
            )
  of (# _, a #) -> a
+
case runRW# (\realWorld -> let (# realWorld1', a1' #) = m1 realWorld
                               (# realWorld2', a2' #) = m2 2 realWorld1'
                               (# realWorld3', a3' #) = m3 2 realWorld2'
                            in (# realWorld3', a3' #)
            )
  of (# _, a' #) -> a'
```

Now there is no common subexpression at all. `m1 realWorld` is not a common subexpression because
`realWorld` is a formal parameter.

Hopefully this example convinces you that `accursedUnutterablePerformIO` is indeed accursed and
unutterable. By inlining absolutely everything, `IO` actions are turned inside-out, and side effects hidden
inside `IO` actions are exposed, which can trigger CSE in unexpected ways and cause an `IO`
action which is supposed to be performed multiple times
to be performed only once. Imagine if the `IO` action in question is one that allocates memory - you'd
be totally screwed. The doc of `accursedUnutterablePerformIO` contains some harsh warnings such as
"when you are not looking it stabs you in the back and aliases all of your mutable buffers", and this
is why.
