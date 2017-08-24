---
title: How Trampoline Works in Scala
date: 2017-08-24
---

Trampoline is a way to make non-tail recursive functions stack-safe. Its Scala implementation is explained by Rúnar Bjarnason in his paper, [Stackless Scala With Free Monads](http://blog.higher-order.com/assets/trampolines.pdf), and his book, [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala). Rúnar's (old) blog also has a [post](https://apocalisp.wordpress.com/2011/10/26/tail-call-elimination-in-scala-monads/) illustrating the idea. In this post I'd like to apply this technique on a few simple, concrete examples, and show step-by-step how it works on these examples and why it is able to make them stack-safe.

The name "trampoline" may sound fancy or even intimidating (to me anyway, when I first learned this concept), but the basic intuition is pretty simple: instead of letting the JVM run a recursive function and push a new frame to the call stack each time the recursion is performed, we rewrite the recursive function in a way that we, rather than the JVM, have control of its execution. During the execution, we will build a structure which is essentially equivalent as the call stack, except that it is built on the heap.

This is best illustrated with examples.

# Factorial

The simple, stack-unsafe factorial function is:

{% highlight scala linenos %}
def unsafeFac(n: Int): Int = {
  if (n == 0) 1
  else n * unsafeFac(n - 1)
}
{% endhighlight %}

(please ignore integer overflow and negative numbers - they are irrelevant to this post)

When we call `unsafeFac(5)`, the call stack will look like this:

<img src="/assets/images/call-stack.png" width='220'>

Since it takes O(n) stack space, calling `unsafeFac` with a large `n` will result in `StackOverflowError`.

To make further discussion easier, let me rewrite the `unsafeFac` function in a slightly more verbose way:

{% highlight scala linenos %}
def unsafeFac(n: Int): Int = {
  if (n == 0) return 1
  else {
    val x = unsafeFac(n - 1)
    return n * x
  }
}
{% endhighlight %}

To apply trampoline to this function, we first create a `TailRec` type (which will also be used by other examples in this post):

{% highlight scala linenos %}
sealed trait TailRec[A] {
  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
}

final case class Return[A](a: A) extends TailRec[A]
final case class Suspend[A](resume: () => TailRec[A]) extends TailRec[A]
final case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
{% endhighlight %}

We then rewrite the original recursive function using the `TailRec` type, in the following manner:
- If the original function returns an `A`, the new function should return a `TailRec[A]`.
- Each `return` in the original function should be wrapped in a `Return`.
- Each recursive call in the original function should be wrapped in a `Suspend`.
- Things we do after the recursive call (in this case, multiply the result by `n`) should be wrapped in a `FlatMap`.

So our new "trampolined" factorial function is:

{% highlight scala linenos %}
def fac(n: Int): TailRec[Int] = {
  if (n == 0) Return(1)
  else FlatMap[Int, Int](Suspend(() => fac(n - 1)), x => Return(n * x))
  // or equivalently:
  // else Suspend(() => fac(n - 1)).flatMap(x => Return(n * x))
}
{% endhighlight %}

The key things to note about the trampolined factorial function are:
1. The `return`s (which pop stack frames) and recursive calls (which push stack frames) are gone - replaced by our own data types, `Return` and `Suspend`. This gives us control of how the new factorial function is executed.
1. The `Suspend` class wraps a *thunk* (a function that takes no parameter). This makes it lazy: when we create a `Suspend`, the function it wraps is not evaluated. The function is only evaluated when we explicitly run it, which means we will only continue the recursion when we wish to do so.

To execute a trampolined function (i.e., to extract the `A` out of a `TailRec[A]`), we use the following tail-recursive `run` function:

{% highlight scala linenos %}
def run[A](tr: TailRec[A]): A = tr match {
  case Return(a) => a
  case Suspend(r) => run(r())
  case FlatMap(x, f) => x match {
    case Return(a) => run(f(a))
    case Suspend(r) => run(FlatMap(r(), f))
    case FlatMap(y, g) => run(y.flatMap(g(_) flatMap f))
  }
}
{% endhighlight %}

Now let's see what happens we we call `run(fac(5))`. As the first step, we need to evaluate `fac(5)`. According to the definition of `fac`, `fac(5)` returns

```scala
FlatMap(Suspend(() => fac(4)), x => Return(5 * x))
```
Note how this `FlatMap` resembles the first frame in the call stack shown above. So now we have
```scala
run(FlatMap(Suspend(() => fac(4)), x => Return(5 * x)))
```

The argument to `run` is now fully evaluated, so we enter the `run` function. Note that we do *not* evaluate `fac(4)` at this point, because, as I just explained above, `fac(4)` is wrapped in a thunk in `Suspend`.

According to the definition of `run` (in particular, line 6), we now have

```scala
run(FlatMap(fac(4), x => Return(5 * x)))
```

Next we need to go back into the `fac` function to evaluate `fac(4)`, which gives us

```scala
run(FlatMap(FlatMap(Suspend(() => fac(3)), x => Return (4 * x)), x => Return(5 * x)))
```

which looks very much like the first two frames of the call stack. Then we return to the `run` function to run this `FlatMap`, and we now have
```scala
run(FlatMap(fac(3), x => FlatMap(Return(4 * x), x => Return(5 * x))))
```

At this point we go back into the `fac` function again to evaluate `fac(3)`, and the computation continues in a similar fashion. In the end, we will have built the following structure:

```scala
FlatMap(
  Return(1),
  x => Flatmap(
    Return(1 * x), x => Flatmap(
      Return(2 * x), x => Flatmap(
        Return(3 * x), x => Flatmap(
          Return(4 * x), x => Return(5 * x)
        )
      )
    )
  )
)
```

This `FlatMap` has the same structure as the call stack, except that it is on the heap. Running this `FlatMap` will give us the desired result, 120, in a stack-safe way.

It should now be obvious why this technique is called "trampoline": during the execution of `run(fac(5))`, we keep jumping back and forth between `run` and `fac`.

# Even and Odd

In this example we use two functions to check whether a number is even or odd:

{% highlight scala linenos %}
def unsafeEven(n: Int): Boolean = {
  if (n == 0) true
  else unsafeOdd(n - 1)
}

def unsafeOdd(n: Int): Boolean = {
  if (n == 0) false
  else unsafeEven(n - 1)
}
{% endhighlight %}

These two functions are in fact tail recursive, but they are *mutually tail recursive*: they are defined in terms of each other. Scala cannot optimize for mutual tail recursions due to limitations of JVM (by contrast, Haskell can, so these two functions are stack-safe in Haskell), so passing a large `n` to either function will cause StackOverflowError.

We can trampoline these two functions in the same way as the factorial function:

{% highlight scala linenos %}
def even(n: Int): TailRec[Boolean] = {
  if (n == 0) Return(true)
  else Suspend(() => odd(n - 1))
}

def odd(n: Int): TailRec[Boolean] = {
  if (n == 0) Return(false)
  else Suspend(() => even(n - 1))
}
{% endhighlight %}

We do not need `FlatMap` in this case because no further step is needed after the tail call. And because no `FlatMap` is involved, it doesn't need to build a structure on the heap like it does for the factorial function. Running `even` and `odd` takes O(1) stack and O(1) heap.

# Fibonacci

Now let's trampline the following Fibonacci function:

{% highlight scala linenos %}
def unsafeFib(n: Int): Int = {
  if (n <= 1) n
  else unsafeFib(n - 2) + unsafeFib(n - 1)
}
{% endhighlight %}

What makes the Fibonacci function slightly more interesting is that it makes two recursive calls in its body, although this doesn't really add any new challenge - the way we trampoline the Fibonacci function is the same as before:

{% highlight scala linenos %}
def fib(n: Int): TailRec[Int] = {
  if (n <= 1) Return(n)
  else Suspend(() => fib(n - 2)).flatMap(x => Suspend(() => fib(n - 1)).flatMap(y => Return(x + y)))
}
{% endhighlight %}

Whatever happens after the first recursive call (`unsafeFib(n - 2)`) is wrapped in the first `flatMap`, and whatever happens after the second recursive call (`unsafeFib(n - 1)`) is wrapped in the second `flatMap`.

Since `flatMap(y => Return(x + y)))` is the same as `map(y => x + y)`, we can simplify the above implementation a little bit:

{% highlight scala linenos %}
def fib(n: Int): TailRec[Int] = {
  if (n <= 1) Return(n)
  else Suspend(() => fib(n - 2)).flatMap(x => Suspend(() => fib(n - 1)).map(x + _))
}
{% endhighlight %}

We can also turn a sequence of `flatMap`s followed by a `map` into Scala's for-comprehension:

{% highlight scala linenos %}
def fib(n: Int): TailRec[Int] = {
  if (n <= 1) Return(n)
  else for {
    x <- Suspend(() => fib(n - 2))
    y <- Suspend(() => fib(n - 1))
  } yield x + y
}
{% endhighlight %}

Again, running the `fib` function will build a structure on the heap similar as the call stack for the `unsafeFib` function.

So having two recursive calls is not so difficult. What about an arbitrary number of recursive calls?

# Map over Tree

In the final example we shall play with a recursive data type representing an n-ary tree:

{% highlight scala linenos %}
sealed trait Tree[A] {
  def label: A
}
final case class Leaf[A](label: A) extends Tree[A]
final case class Node[A](label: A, children: List[Tree[A]]) extends Tree[A]
{% endhighlight %}

The following is a recursive function that maps on the labels of tree nodes:

{% highlight scala linenos %}
def unsafeTreeMap[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
  case Leaf(a) => Leaf(f(a))
  case Node(a, children) => Node(f(a), children.map(unsafeTreeMap(_, f)))
}
{% endhighlight %}

Using the same approach as before: wrapping `return`s in `Return`s and wrapping recursive calls in `Suspend`s, this is what we get:

{% highlight scala linenos %}
def treeMap[A, B](tree: Tree[A], f: A => B): TailRec[Tree[B]] = tree match {
  case Leaf(a) => Return(Leaf(f(a)))
  case Node(a, children) =>
    val ltt: List[TailRec[Tree[B]]] = children.map(child => Suspend(() => treeMap(child, f)))
    ???
}
{% endhighlight %}

The `Leaf` case is trivial, but in the `Node` case, what should we do after getting a `List[TailRec[Tree[B]]]`? It's not quite obvious. It would be much better if we had a `TailRec[List[Tree[B]]]` instead; if that's the case we can simply proceed with `flatMap` to get what we want:

{% highlight scala linenos %}
def treeMap[A, B](tree: Tree[A], f: A => B): TailRec[Tree[B]] = tree match {
  case Leaf(a) => Return(Leaf(f(a)))
  case Node(a, children) =>
    val ltt: List[TailRec[Tree[B]]] = children.map(child => Suspend(() => treeMap(child, f)))
    val tlt: TailRec[List[Tree[B]]] = ???
    tlt.flatMap(lt => Return(Node(f(a), lt)))
    // or equivalently: tlt.map(Node(f(a), _))
}
{% endhighlight %}

It turns out that converting a `List[TailRec[Tree[B]]]` to a `TailRec[List[Tree[B]]]` is a standard operation in functional programming, known as `sequence`. Scala has a `sequence` method for `Future`, but not for `List`. Multiple open-source libraries provide the `sequence` method for `List` as part of the `Traverse` type class, including [Scalaz](https://static.javadoc.io/org.scalaz/scalaz-core_2.11/7.2.15/index.html#scalaz.Traverse), [Cats](https://typelevel.org/cats/api/cats/Traverse.html) and [Structures](https://github.com/mpilquist/Structures/blob/master/core/shared/src/main/scala/structures/Traverse.scala). Here let's just implement our own version:

{% highlight scala linenos %}
def sequence[A](ltt: List[TailRec[A]]): TailRec[List[A]] =
  ltt.reverse.foldLeft(Return(Nil): TailRec[List[A]]) { (tla, ta) =>
    ta map ((_: A) :: (_: List[A])).curried flatMap tla.map
  }
{% endhighlight %}

Now we can complete our implementation of the trampolined `treeMap`:

{% highlight scala linenos %}
def treeMap[A, B](tree: Tree[A], f: A => B): TailRec[Tree[B]] = tree match {
  case Leaf(a) => Return(Leaf(f(a)))
  case Node(a, children) =>
    val ltt: List[TailRec[Tree[B]]] = children.map(child => Suspend(() => treeMap(child, f)))
    val tlt: TailRec[List[Tree[B]]] = sequence(ltt)
    tlt.map(Node(f(a), _))
}
{% endhighlight %}

# A Final Word on Laziness and Stack-Safety

The key reason why trampolined functions are stack-safe is because `Suspend` is lazy, in other words, the recursion happens in a lazy structure. Generally speaking, lazy recursions tend to be stack-safe, even if they are not tail recursions. For example, the following function:

```scala
def func(x: Int): Stream[Int] = x #:: func(x + 1)
```

is stack safe, even though it is not tail recursive, because the tail of a `Stream` is lazy. Calling

```scala
println(func(1).take(n).toList)
```

with a large `n` will not cause `StackOverflowError`. The execution simply trampolines between `func` and `take`.
