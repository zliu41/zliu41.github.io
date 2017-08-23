---
title: Arrow Hangman in Scala
date: 2017-08-23
---

`Arrow` is a useful typeclass for modeling something that behave like functions, e.g,. `Function1`: `A => B`, `Kleisli`: `A => M[B]`  (also known as `ReaderT`), `Cokleisli`: `F[A] => B`, etc. So useful, that Haskell provides a `proc` notation for composing and combining `Arrow`s, similar as the `do` notation for sequencing monadic operations. The `proc` notation comes in really handy, since without it, `Arrow`s have to be used point-free, which impairs readability.

The Haskell Wikibook has a [nice little example](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial) of a Hangman implementation based on an `Arrow` type called `Circuit`. `Circuit` is basically a fancy function which takes an `A`, and in addition to returning a `B`, it also returns a new `Circuit[A, B]`. This sounds similar as the `State` monad (which returns a result and a new `State` from an initial `State`), and indeed, `Circuit` can be used to perform stateful computations.

Both Scalaz and Cats provide the `Arrow` typeclass. I wrote a [Scala implementation of Arrow Hangman](https://gist.github.com/zliu41/55fd7f68e43c56f38e7f63aae04fe5ff) using Scalaz, which closely follows the Haskell implementation, except that Scala has no `proc` notation (although [it is possible to create one](https://github.com/todesking/arrow_builder.scala)), so we resort to raw combinators like `>>>`, `&&&` and `***`. It's not too bad though, as long as we give names to intermediate steps, rather than doing all the combination in one huge expression.
