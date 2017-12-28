---
title: Recursion Schemes in Scala - An Absolutely Elementary Introduction
date: 2017-11-13
---


When writing any non-trivial amount of code, we usually factor out the common logic or patterns to improve modularity, facilitate code reuse and avoid repeating ourselves. Recursion schemes is just about that. With recursion schemes you can factor out the recursion patterns in your code, so that you don't need to write the same types of recursion over and over again. This may sound a little magical to those who aren't familiar with recursion schemes, but it really isn't.

This post aims to provide an absolutely elementary introduction to recursion schemes using Scala. There are a few good introductory posts on recursion schemes in Haskell, but I haven't seen one for Scala, so here we go.

In this post I will use a single example, the factorial function, throughout the post. Calculating factorials is by no means the most striking use case of recursion schemes, but it is suitable for an introductory post. The purpose of this post is not to convince anyone that recursion schemes are useful in the real world - that has already been proven beyond doubt. The purpose is to explain how recursion schemes work to those who are unfamiliar with the concept, in the simplest possible way. To that end, a simple, linear-recursive, toy example like factorial works well in my opinion. The resources in the [Further Reading](#further-reading) section offer more real-world examples for those interested.

For the same reason, the implementation of recursion schemes in this post is a toy implementation - it is not stack-safe, not as general as it can be, and cannot handle infinite recursions. It just serves the purpose of a basic introduction of how things work. If you are interested in a production-ready Scala implementation of the recursion schemes, you'll want to look at the [Matryoshka](https://github.com/slamdata/matryoshka) project.

# A Recap of Fixpoint Combinator and Fixpoint Type

Recursion schemes are based on fixpoints (also known as fixed points). So before introducing recursion schemes, let's have a quick recap of how fixpoint combinators and fixpoint types work.

## Fixpoint Combinator

One of my previous posts, [The fix Combinator in Scalaz](http://free.cofree.io/2017/08/28/fixpoint/), contains an introduction of the fixpoint combinator. A fixpoint combinator `fix` is a combinator such that for all functions `f`, `fix(f) = f(fix(f))`. In other words, `fix(f)` is the fixpoint of `f`. This allows you to convert a recursive function into a non-recursive function, by delegating the recursion to `fix`. The way to do it is: suppose you have a recursive function `f`:

```
f(params) = <body containing f>
```

Create a function `g` that takes `f` itself and `f`'s parameters as its parameters, with the same body as `f`:

```
g(f)(params) = <body containing f>
```

Now, `g` is no longer recursive because its body doesn't contain `g`. But in order to use `g`, we need to find an `f` that we can feed to `g`. How do we find it? Suppose what we want to find is `x`. By feeding it to `g`, we get

```
g(x)(params) = <body containing x>
```

So `g(x)` is a function that takes `f`'s parameters, and returns `<body containing x>`. Since `g(x)` should be equivalent as `f`, we should have

```
g(x)(params) = <body containing g(x)>
```

which means we need an `x` such that `x = g(x)`. Therefore, one possible `x` is `fix(g)`. This is how you use the fixpoint combinator to factor recursion out of a recursive function, leaving only the "core logic" in the original function.

## Fixpoint Type

The way fixpoint types work is exactly the same as the fixpoint combinator, except that they work at the type level. The following is a simple fixpoint type:

```scala
final case class Fix[F[_]](unfix: F[Fix[F]])
```

That is, given a `Fix` of `F`, you can always get an `F` of `Fix[F]`, and vice versa. This can be used to factor recursion out of recursive _types_, similar as how we factor recursion out of recursive functions as explained above.

Suppose you have a recursive type `F`:

```scala
sealed trait F[Params]
final case class A[Params](a: <type containing F>) extends F[Params]
```

You can create a type `G` that takes `F` itself and `F`'s type parameters as its type parameters:

```scala
sealed trait G[F, Params]
final case class GA[F, Params](a: <type containing F>) extends G[F, Params]
```

Then, `Fix[G]` is a type isomorphic to `F`, but without the explicit recursion as `F` does (note that this is not quite accurate - Scala does not support partially applied types so you cannot directly write `Fix[G]` - but for the purpose of this explanation you can temporarily assume that it does).

Recursion schemes is all about fixpoint types. Next let's review a few different types of recursion schemes using the factorial example.

# Anamorphism

Anamorphism is one of the basic types of recursion schemes. It generalizes unfold, meaning you can use it to create a recursive structure based on a recursive type.

In a previous post, [How Trampoline Works in Scala](http://free.cofree.io/2017/08/24/trampoline/), I explained how to make the factorial function stack-safe using `TailRec`, which is a recursive type. The approach is basically building a structure on the heap using `TailRec` which mimics the call stack. For the sake of this post, we'll use a simpler recursive type to represent the stack:

```scala
sealed trait StackR
final case class DoneR(result: Int = 1) extends StackR
final case class MoreR(stack: StackR, next: Int) extends StackR
```

where `R` stands for "recursive". This is a recursive type because one of the fields in `MoreR` is of type `StackR`.

We can use the following function to generate a structure representing the call stack for calculating the factorial of `n`:

```scala
def unfoldStackR: Int => StackR =
  n => if (n > 0) MoreR(unfoldStackR(n - 1), n) else DoneR()

unfoldStackR(5)
// res0: StackR = MoreR(MoreR(MoreR(MoreR(MoreR(DoneR(1),1),2),3),4),5)
```

Now we wish to build a similar structure using `Fix` and anamorphism, instead of explicit recursion. To do so, as explained before, we create a new type that adds a type parameter to `StackR`. We'll call it `Stack`:

```scala
sealed trait Stack[A]
final case class Done[A](result: Int) extends Stack[A]
final case class More[A](a: A, next: Int) extends Stack[A]
// I don't know why these two "final"s have different colors. It's annoying.
```

To use recursion schemes, `Stack` must be a functor, so we add a functor instance for `Stack`, together with two smart constructors to improve type inference.

```scala
import cats.Functor
import cats.implicits._

object Stack {
  implicit val stackFunctor: Functor[Stack] = new Functor[Stack] {
    override def map[A, B](sa: Stack[A])(f: A => B): Stack[B] =
      sa match {
        case Done(result) => Done(result)
        case More(a, next) => More(f(a), next)
      }
  }

  def done[A](result: Int = 1): Stack[A] = Done(result)
  def more[A](a: A, next: Int): Stack[A] = More(a, next)
}
```

As we can see, the field of case class `Done` does not use type `A`, so mapping over `Done[A]` is a no-op. `Done` is called the base case (or degenerate case, terminating case) for the `Stack` type. This is what terminates the recursion. If a recursive type does not have a base case, then if a value of that type exists, it must be infinitely recursive.

Anamorphism is a type of recursion scheme, which takes a function of type `A => F[A]` (also known as a Coalgebra) where `F` is a functor, and returns a function of type `A => Fix[F]`, that takes an `A` and unfolds it into a recursive structure, `Fix[F]`. The implementation is quite simple if guided by the type signature, in fact it is the only sensible implementation that compiles:

```scala
// Type A => F[A] is also known as Coalgebra.
def ana[F[_] : Functor, A](f: A => F[A]): A => Fix[F] =
  a => Fix(f(a) map ana(f))
```

That is, we only need to supply a coalgebra, `A => F[A]` which does not need to be recursive, in order to generate the recursive structure. This is what we mean by "factoring recursion out": your coalgebra only needs to contain the core business logic, and the recursion is taken care of by `ana`.

The coalgebra for factorial would be:

```scala
val stackCoalgebra: Int => Stack[Int] =
    n => if (n > 0) more(n - 1, n) else done()
```

Feeding `stackCoalgebra` to `ana` should build a structure isomorphic to the one built by `unfoldStackR`:

```scala
ana(stackCoalgebra).apply(5)
// res1: Fix[Stack] = Fix(More(Fix(More(Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3)),4)),5))
```

The `.apply` above, unfortunately, cannot be ommitted, because `ana` has a context bound on `F`, i.e., `F[_] : Functor`, which is desugared into an implicit parameter list. So if you write `ana(stackCoalgebra)(5)`, Scala will try to match `5` with the implicit parameter, `Functor[F]`, which will give a compilation error.

# Catamorphism

Previously given an integer `n`, we unfold it into a stack structure representing the computation of the factorial of `n`. Now we need to fold it back into an integer, the factorial of `n`. To do so we can use catamorphism, which is a generalized fold.

Let's first have a look at what it's like to fold the stack using explicit recursion:

```scala
// Explicit recursion with StackR
def foldStackR: StackR => Int = {
  case DoneR(result) => result
  case MoreR(acc, next) => foldStackR(acc) * next
}

// Explicit recursion with Fix[Stack]
def foldFixStack: Fix[Stack] => Int =
  _.unfix match {
    case Done(result) => result
    case More(fix, next) => foldFixStack(fix) * next
  }

(unfoldStackR andThen foldStackR)(5)
// res2: Int = 120

(ana(stackCoalgebra) andThen foldFixStack)(5)
// res3: Int = 120
```

Catamorphism is the dual of anamorphism, and can be implemented by reversing the arrows in `ana`:

```scala
// Type F[A] => A is also known as Algebra.
def cata[F[_] : Functor, A](f: F[A] => A): Fix[F] => A =
  fix => f(fix.unfix map cata(f))
```

As with `ana`, we only need to provide an algebra, `F[A] => A`, and the recursion is taken care of by `cata`. The algebra for folding the stack would be

```scala
val stackAlgebra: Stack[Int] => Int = {
  case Done(result) => result
  case More(acc, next) => acc * next
}

(ana(stackCoalgebra) andThen cata(stackAlgebra))(5)
// res4: Int = 120
```

# Hylomorphism

Hylomorphism is the composition of anamorphism and catamorphism, and can be implemented simply as

```scala
def hyloSimple[F[_] : Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
  ana(g) andThen cata(f)
```

`hyloSimple` first uses `ana` to build a potentially large structure, before using `cata` to tear it down. Alternatively, we can implement hylomorphism directly without using `ana` or `cata`, which "fuses" the anamorphism and the catamorphism, meaning at no time do we have a large `Fix` structure in memory.

```scala
def hylo[F[_] : Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
  a => f(g(a) map hylo(f)(g))

hylo(stackAlgebra)(stackCoalgebra).apply(5)
// res5: Int = 120
```

To see how `hylo` behaves differently than `hyloSimple`, note that `hylo` does not involve `Fix` in its implementation, so it doesn't create any `Fix`. All it does is to create and map on `More[Int]` and `Done[Int]`, such as `Done(1)`, `More(1, 2)`, `More(2, 3)`, etc.

# Paramorphism

Paramorphism is also a generalization of fold. It is an extension of catamorphism, and offers more power. Here's the type signature and implementation:

```scala
def para[F[_] : Functor, A](f: F[(Fix[F], A)] => A): Fix[F] => A =
  fix => f(fix.unfix.map(fix => fix -> para(f).apply(fix)))
```

We can implement `cata` in terms of `para`:

```scala
def cataViaPara[F[_] : Functor, A](f: F[A] => A): Fix[F] => A =
  para(((_: F[(Fix[F], A)]).map(_._2)) andThen f)
```

Paramorphism is more powerful than catamorphism in the sense that in the algebra `f`, we not only have an `F[A]` to work with, but we also have an `F[Fix[F]]`, which means we have access to the `Fix` structure that yields the `A` when being folded.

For example, if we use `para` to fold our stack corresponding to the calculation of the factorial of 5, then instead of seeing `More(acc=24, next=5)` in one of the steps, we will also have access to the `Fix` structure that generated 24, i.e., 

```
Fix(More(Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3)),4))
```

This is not particularly useful for calculating factorials using our `Stack` type, because `Stack` already stores the accumulated value `acc` in `More`. However, paramorphism does allow us to calculate factorials using the following simpler type, `NatR`, representing natural numbers:

```scala
sealed trait NatR
case object ZeroR extends NatR
final case class SuccR(prev: NatR) extends NatR
```

Same as before, we add a type parameter for `NatR` to get rid of the explicit recursion. We call it `Nat`, and implement its functor instance.

```scala
sealed trait Nat[A]
final case class Zero[A]() extends Nat[A]
final case class Succ[A](a: A) extends Nat[A]

object Nat {
  implicit val natFunctor: Functor[Nat] = new Functor[Nat] {
    override def map[A, B](na: Nat[A])(f: A => B): Nat[B] =
      na match {
        case Zero() => Zero()
        case Succ(a) => Succ(f(a))
      }
  }
}
```

The `Succ` type has one less field compared to `More`, but it can be recovered using the `Fix` structure that we have access to thanks to paramorphism.

```scala
val natAlgebraPara: Nat[(Fix[Nat], Int)] => Int = {
  case Zero() => 1
  case Succ((fix, acc)) =>
    ??? * acc
}
```

We need to convert a `Fix[Nat]` to an `Int`, representing the next integer to be multiplied, and replace `???` with it. To do so, we can use `cata` by feeding it an algebra from `Nat[Int]` to `Int`:

```scala
val natAlgebra: Nat[Int] => Int = {
  case Zero() => 1
  case Succ(n) => n + 1
}

val natAlgebraPara: Nat[(Fix[Nat], Int)] => Int = {
  case Zero() => 1
  case Succ((fix, acc)) =>
    cata(natAlgebra).apply(fix) * acc
}
```

We can now test it by using `ana` to generate a `Fix[Nat]` and then use `para` to fold it:

```scala
val natCoalgebra: Int => Nat[Int] =
  n => if (n == 0) Zero() else Succ(n - 1)

(ana(natCoalgebra) andThen para(natAlgebraPara))(5)
// res6: Int = 120
```

# Apomorphism

Apomorphism is the dual of paramorphism, and is an extension of anamorphism.

```scala
def apo[F[_] : Functor, A](f: A => F[Either[Fix[F], A]]): A => Fix[F] =
  a => Fix(f(a) map {
    case Left(fix) => fix
    case Right(aa) => apo(f).apply(aa)
  })
```

Note that in addition to reversing the arrows in `para`, we also change `F[(Fix[F], A)]` to `F[Either[Fix[F], A]]`, since the dual of a pair (product type) is an `Either` (sum type).

Not surprisingly, `ana` can be implemented in terms of `apo`:

```scala
def anaViaApo[F[_] : Functor, A](f: A => F[A]): A => Fix[F] =
  apo(f andThen (_.map(_.asRight[Fix[F]])))
```

Compared to anamorphism, apomorphism gives you more control on when to stop the recursion. In anamorphism, the recursion is terminated when a node representing the base case (e.g., `Done` or `Zero`) is visited, on which the `map` function is a no-op. In apomorphism, the recursion can be terminated either by visiting a base case, or if `f` returns a `Left` containing a `Fix[F]`.

In our factorial example with `Stack`, using anamorphism, the recursion terminates at `n = 1` where `stackCoalgebra` returns the base case `Done`. Suppose we already have a `Fix` structure corresponding to `n = 3`, i.e., the last three steps of the fold:

```scala
val lastThreeSteps: Fix[Stack] = Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3))
```

and we want the recursion to terminate on `n = 3`. We can use apomorphism to achieve that, by writing the corresponding coalgebra:

```scala
val stackCoalgebraApo: Int => Stack[Either[Fix[Stack], Int]] =
  n => if (n > 3) more(n - 1, n).map(_.asRight) else lastThreeSteps.unfix.map(_.asLeft)
```

When calling `apo(stackCoalgebraApo).apply(5)`, the recursion stops at `n = 3`, where `lastThreeSteps` is returned, instead of continuing the recursion.

# Histomorphism

Histomorphism is yet another recursion scheme for fold, and is more powerful than paramorphism. Histomorphism operates on an enhanced version of `Fix`, called `Cofree`, where each node in the structure is annotated by some value.

```scala
final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
```

The implementation of histomorphism requires a helper function `toCofree` to convert a `Fix[F]` to a `Cofree[F, A]`, where each node is annotated by the value generated by folding the corresponding `Fix[F]` structure:

```scala
def histo[F[_] : Functor, A](f: F[Cofree[F, A]] => A): Fix[F] => A = {
  def toCofree: Fix[F] => Cofree[F, A] =
    fix => Cofree(head = histo(f).apply(fix), tail = fix.unfix map toCofree)

  fix => f(fix.unfix map toCofree)
}
```

Recall that in catamorphism, at each step of the fold, you only have the value of the current fold, `F[A]`. In paramorphism, you additionally have access to the structure that generated that value, `F[Fix[F]]`. And in histomorphism, additionally, you also have the history of the values generated by the fold so far, or the history of the computation if you will.

In the factorial example, in the step corresponding to `n = 5`, in catamorphism we have `More(acc=24, next=5)`; in paramorphism we also have the structure that generated 24, i.e., `Fix(More(Fix(More(Fix(More(Fix(Done(1)),2)),3)),4))`; in histomorphism we additionally have the history of the generated values, i.e., 1, 1, 2, 6, 24. Each of these values is encoded as the head of a `Cofree` structure, together with the corresponding node, which is in the tail.

This history of generated values is not really useful for the factorial example, since all we need is the current value, 24, and the next number to be multiplied, 5. But it should be easy to imagine that this can be useful in many other cases.

# Dynamorphism

Dynamorphism is the composition of anamorhpism and histomorphism. Similar as hylomorphism, it can be implemented either via `ana` and `histo`:

```scala
def dynaSimple[F[_] : Functor, A, B](f: F[Cofree[F, B]] => B)(g: A => F[A]): A => B =
  ana(g) andThen histo(f)
```

or directly:

```scala
def dyna[F[_] : Functor, A, B](f: F[Cofree[F, B]] => B)(g: A => F[A]): A => B = {
  val cofree: F[Cofree[F, B]] => Cofree[F, B] =
    fc => Cofree(f(fc), fc)
  a => hylo(cofree)(g).apply(a).head
}
```

The latter implementation avoids building the whole `Fix` structure.

# Futumorphism

The last recursion scheme we are going to cover in this post is futumorphism. It sounds like the dual of histomorphism, and it indeed is. The dual of `Cofree` is, unsurprisingly, the following `Free` type:

```scala
sealed trait Free[F[_], A]
final case class Continue[F[_], A](a: A) extends Free[F, A]
final case class Combine[F[_], A](fa: F[Free[F, A]]) extends Free[F, A]

object Free {
  def continue[F[_], A](a: A): Free[F, A] = Continue(a)
  def combine[F[_], A](fa: F[Free[F, A]]): Free[F, A] = Combine(fa)
}
```

Each `Cofree` has a recursive structure tagged with a value of type `A`, while each `Free` has either a recursive structure, or a tag with a value of type `A`.

Given `Free`, here's the definition and implementation of futumorphism:

```scala
def futu[F[_] : Functor, A](f: A => F[Free[F, A]]): A => Fix[F] = {
  def toFix: Free[F, A] => Fix[F] = {
    case Continue(a) => futu(f).apply(a)
    case Combine(fa) => Fix(fa map toFix)
  }

  a => Fix(f(a) map toFix)
}
```

Futumorphism is another unfold scheme, and is a more powerful one than apomorphism. Recall that in apomorphism, given a value of `A`, you either choose to continue the recursion by returning a `Right`, or choose to stop the recursion by returning a `Left`. In futumorphism, you can also choose to continue the recursion by returning a `Continue` or stop by returning a `Combine`. Additionally, you will be able to unfold multiple steps at a time, which you cannot do with apomorphism.

In anamorphism, to build the stack corresponding to the computation of the factorial of 5, you'll need 5 recursive steps. In apomorphism, as we've seen earlier, you can effectively combine the last three steps together, by terminating the recursion at `n = 3` and returning `lastThreeSteps`. But once the recursion is terminated, it's terminated. You cannot, for example, combine the _first_ three steps together, and then resume the recursion. On the other hand, you can do that with futumorphism.

The reason you can do that with futumorphism is because a `Free` is either a single `A` wrapped in `Continue` or a recursive structure wrapped in `Combine`. To combine multiple steps together, just have your coalgebra return a `Combine` wrapping a recursive structure corresponding to those steps; to resume the normal recursion, just have it return a `Continue` wrapping a single `A`.

As an example, suppose we have the following recursive structure corresponding to the first three steps in building the stack corresponding to the factorial of 5 (i.e., `n = 5, 4, 3`):

```scala
val firstThreeSteps: Stack[Free[Stack, Int]] = more(combine(more(continue(3), 4)), 5)
```

Then we can combine these three steps together, and resume the normal recursion at `n = 2`, using the following coalgebra:

```scala
val stackCoalgebraFutu: Int => Stack[Free[Stack, Int]] =
  n =>
    if (n == 5) firstThreeSteps
    else if (n > 0) more(n - 1, n) map continue
    else done() map continue

futu(stackCoalgebraFutu).apply(5)
// res7: Fix[Stack] = Fix(More(Fix(More(Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3)),4)),5))
```

# Conclusion

Even though the factorial example used in this post is exceedingly simplistic and by no means shows the full power of recursion schemes, it still reveals a few benefits of using recursion schemes over explicit recursion.

Basically, as said before, it allows you to factor your recursion patterns out and only keep the core business logic in your program. This is usually beneficial but not always the case, depending on the problem you are solving, and more importantly your team. If other people in your team have also studied recursion schemes, then your collaboration can become significantly more productive. You simply mention "I'm doing histomorphism" and your teammates will immediately understand what kind of recursion you are doing. On the other hand, if you are the only person in the team that has studied it, and your teammates either do not have the time or do not have the desire to do so, then it could make your code harder to read or maintain by others.

Another benefit of factoring recursion out is that it helps eliminate bugs. In reality you often need to deal with recursive types which are much more complex than `Stack` or `Nat`. If you use explicit recursion, there will be a lot of recursive calls in your program, and if you forget one, your code may still typecheck, but will not have the correct behavior at runtime. Using recursion schemes, there will be no explicit recursion in your program so this class of bug is eliminated.

Last but not the least, it can sometimes significantly shorten your code. When you have a recursive type much more complex than `Stack` or `Nat`, it is possible that you sometimes need to write blocks of boilerplate code solely for the purpose of calling the recursion. If you take recursion out, it becomes no-op, and the whole block can be removed.

On a side note, the recursion scheme implementation in this post is not as general as it can be - for example, all recursion schemes in this post return either `A => Fix[A]` or `Fix[A] => A`, but `Fix` is only one of the possible types you can work with using recursion schemes. There are also lots of subtle details in a production-ready implementation (such as Matryoshka), especially for Scala, that this post does not cover.

The code shown in this post can be found [here](https://gist.github.com/zliu41/4d3e5a440339181bfdef57af743c0a1d).

# Further Reading

- The classic paper on recursion schemes: [Functional Programming with Bananas,Lenses, Envelopes and Barbed Wire](https://pdfs.semanticscholar.org/5db3/c6793c07285bf0f5e95fe5a25f53e7488051.pdf)
- [Matryoshka](https://github.com/slamdata/matryoshka), a Scala recursion scheme library. The README of the library also contains a few good external resources.
- Zainab Ali's [talk](https://www.youtube.com/watch?v=TrPlVnoLxTI) at Scala World 2017 on recursion schemes.
- Greg Pfeil's [talk](https://www.youtube.com/watch?v=lQdpXqD7Uic) at Scala By The Bay 2016 on the Matryoshka library.
- A [series of blog posts](http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/) on recursion schemes in Haskell

