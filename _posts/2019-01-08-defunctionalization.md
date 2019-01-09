---
title: Defunctionalization for Haskell Type Families
date: 2019-01-08
---

The first language I used to do purely functional programming is Scala, which I learned when I was at Facebook, and I absolutely loved it. Not long afterwards
I started to learn Haskell, and sure enough, Scala quickly became the second favorite. Haskell
is simply too beautiful. This motivated me to join Formation in April last year. Being paid to write Haskell on a daily basis feels like a dream, in the sense that my work is now my hobby. As a result, I'll start to write more Haskell posts here than Scala ones. I still like Scala but I haven't touched it in a while and I'm fairly rusty at this point.

The style of my posts, however, will remain unchanged: I'm committed to making them accessible to beginner-level functional programmers. To this end, there will continue to be plenty of examples that are as simple as I can make them, as well as explanation of the intuition.

In this post I'd like to introduce a neat hack in Haskell: defunctionalization for type families. I first learned about this technique
from Sandy Maguire's book "[Thinking with Types](https://leanpub.com/thinking-with-types)", although
I'm going to discuss a slightly simplified approach in this post. It is a hack because it is a workaround for not having unsaturated type families or type-level lambdas in Haskell. Nonetheless, it is completely safe (unlike many other hacks we've heard of), and can be super useful in type-level programming, so I think it is definitely worth knowing.


# TL;DR

[Defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization), as explained in Wikipedia, is a technique for eliminating higher-order functions. And what we want to do is basically
to use it to eliminate higher-order functions at type-level. Why do we want to do that? Because although Haskell is a functional language, and ordinary functions (also referred to as term-level functions or value-level functions) are first class (i.e., can be assigned to variables, passed as arguments to functions and returned from functions), type-level functions (i.e., type families) are _not_ first class, and higher-order type-level functions are not supported. So, whenever we want to write
or use a higher-order type-level function (such as the type-level `fmap`), we must use this defunctionalization thing to rewrite it to something equivalent and supported.

Let's introduce some basic concepts first.

# Basic Concepts

## Type Families

Type-level functions in Haskell are called "type families". Type families can be defined in a few different ways, such
as closed type families and open type families. There is a third way called associated type families, which are
type-level functions defined within type classes. It is more commonly used than the first two, but is less relevant
to our topic of defunctionalization so we omit it here.

### Closed Type Families

In a closed type family, the entire type-level function is defined the moment you
introduce the type family, under the `where` clause. For example,

```haskell
import Data.Kind (Constraint, Type)

type family Foo (x :: Type) (y :: Type) :: Type where
  Foo Int Bool = Double
  Foo Int Double = Maybe Char
  Foo String (Maybe a) = Either Int a
  Foo String (Maybe Int) = Maybe String  -- Warning: overlapped with the line above
```

This is a type-level function that takes two arguments, both of kind `Type`, and returns
a result, also of kind `Type`. For example, it maps `(Int, Bool)` to `Double`. So now you can
write `x = 4.2 :: Foo Int Bool`, where `Foo Int Bool` becomes a synonym of `Double`.

Equations in a closed type family are applied top-down, so
the last equation above, involving `Foo String (Maybe Int)`, causes a GHC warning, since
it overlaps with the line above and is unreachable. However, if we swap the last two
equations, then the warning disappears, since `Maybe a` is more general than `Maybe Int`.

Note that you don't need to make a closed type family definition "total" as you would
for term-value functions, because the compiler will automatically add an equation
`Foo a b = Foo a b` in the end, if such an equation isn't explicitly defined.

Of course, the kinds of the arguments and the result don't have to be of kind `Type` - they can be any
kind. For example, the following type family maps type constructors to constraint constructors:

```haskell
type family Bar (x :: Type -> Type) (y :: Type -> Type -> Type) :: (Type -> Type) -> Constraint where
  Bar Maybe Either = Functor
  Bar [] (->) = Applicative
```

Type families `Foo` and `Bar` are just for illustration, and are completely useless. Here's a more useful
type family, taking two arguments of kind `Bool` and returns their conjunction:

```haskell
type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True y = y
  And 'False y = 'False
```

Also note that type constructors are a special case of type-level functions. Type constructors also
map types to types, but the result types are determined by the type constructors and can't be changed.
For example, the `Maybe` type constructor always maps `Int` to `Maybe Int`.

### Open Type families

In an open type family, the type-level function is defined by `type instance` declarations
which can be extended. For example,

```haskell
type family Baz (x :: Type) (y :: Type) :: Type
type instance Baz Int Bool = Double
type instance Baz Int (Maybe a) = Either Int a
```

You can import the module that defines `Baz`, and extend it by defining new type instances.
Because of that, there's no such thing as "top-down" in open type families. Therefore, open type families cannot have overlapping type
instances. It is illegal to define `type instance Baz Int (Maybe Int)` or `type instance Baz a b` given
the type instances above.

## Higher-Order Functions

Higher-order functions are functions that take functions as arguments, and/or return functions as
results.

The following is a simple higher-order function. It takes a function of type `a -> a`, and a value
of type `a`, and applies the function to the value twice.

```haskell
twice :: (a -> a) -> a -> a
twice f x = f (f x)
```

Haskell, being a purely functional language, relies heavily on higher-order functions. Without higher-order functions,
we can't do things like `fmap` or `>>=`, rendering the entire language almost useless. The same can be said about
type-level programming. Without higher-order type-level functions, type-level programming is still useful but the
usability is severely limited. We won't be able to do type-level `fmap`, or the type-level counterpart of the `twice`
function above. So, can we have higher-order type-level functions or not?

## Higher-Order Type Families?

Unfortunately, such things don't exist in Haskell. To prove it, let's try to write a type-level
`twice` function.

```haskell
type family Twice (f :: Type -> Type) (x :: Type) :: Type where
  Twice f x = f (f x)
```

So far so good. We'd expect that we can apply `Twice` to `(Foo Int)` and `Bool`, and
it would return `Maybe Char` back. Unfortunately, it won't be the case:

```
λ> x = Just 'a' :: Twice (Foo Int) Bool
```

This will not compile:

```
The type family ‘Foo’ should have 2 arguments, but has been given 1
```

The type family `Foo` takes two `Types` and returns a `Type`, and it refuses to take less than two arguments. For example, we can't
treat `Foo Int` as a type-level function from `Type` to `Type`, or treat `Foo` itself as
a type-level function from `(Type, Type)` to `Type`. Occurrences of `Foo` must simply be accompanied
by two arguments. In other words, type families in Haskell cannot be partially applied (or "unsaturated"). This is the case
with both open and close type families.

The reason unsaturated type families are not allowed has to do with the fact that
type families in Haskell are, in general, not injective or generative, where:

- Injectivity: `F a ~ F b ⇒ a ~ b`
- Generativity: `F a ~ G b ⇒ F ~ G ∧ a ~ b`

The `~` symbol denotes (extensional) type equality, so `a ~ b` means a value of type `a`
can be used in any context that expects a value of type `b`.

GHC's type inference relies on the assumptions of injectivity and generativity to work, and unsaturated type
families violate these assumptions. For example, if `m a ~ Maybe Int`, and `m` is possibly
an unsaturated type family, GHC won't be able to infer `m ~ Maybe` or `a ~ Int`.

This means there's _no way_ to apply `Twice` to a type family. We can, however,
apply it to a type constructor, since type constructors are both injective
and generative, and thus _can_ be partially applied:

```
λ> x = Left 42 :: Twice (Either Int) Bool
λ> :t x
x :: Either Int (Either Int Bool)
```

But this is not very useful, so it is a severe limitation. And of course, attempting to write a type-level lambda:

```haskell
type family TwiceCurried (x :: Type) :: Type -> Type where
  TwiceCurried f = \x -> f (f x)
```

will fail miserably, since there's no such thing (yet) as type-level lambdas in Haskell.
A type family returning a `Type -> Type` can only return type constructors, e.g., `Maybe`.

# Defunctionalization

Ok, so defunctionalization to the rescue. The key idea here is to take advantage of
type constructors, since as just mentioned, type constructors, unlike type families,
_can_ be partially applied, and you _can_ pass an unsaturated type constructor to a higher-order type family (such as `Twice`).

So the plan of attack is this: we are going to encode the type-level function, which we intend to
pass to a higher-order type family, as a type constructor. Then, we create another type family to
_evaluate_ the type constructor (i.e., map it to the correct types). Finally, we modify
our higher-order type family to use the evaluator.

If this sounds confusing, our running example with `Twice` and `Foo` should help clarify things. We start by turning
`Foo` into a type constructor:

```haskell
data Foo' (x :: Type) (y :: Type)
```

The type family `Foo` is a type-level function from `(Type, Type)` to `Type`, so we make `Foo'` take
two type parameters, corresponding to the arguments to `Foo`. So the kind of both `Foo` and `Foo'` is `Type -> Type -> Type`.
There's no need for `Foo'` to have data constructors since we won't need to create values of `Foo'`.

We can also define `Foo'` using GADT syntax, i.e.,

```haskell
data Foo' :: Type -> Type -> Type
```

This way we don't need to name the type parameters, which may or may not be desirable.

Now we need to define the equations in the `Foo` type family. Since `Foo` maps
`(Int, Bool)` to `Double`, we need a way to map `Foo' Int Bool` to `Double`. This requires a
type-level function, so we can use an open type family, like this:

```haskell
type family Eval (x :: Type) :: a
type instance Eval (Foo' Int Bool) = Double
type instance Eval (Foo' Int Double) = Maybe Char
-- the rest is omitted
```

This is the evaluator we mentioned before.

And then we modify the definition of `Twice` to use `Eval`:

```haskell
type family Twice' (f :: Type -> Type) (x :: Type) :: Type where
  Twice' f x = Eval (f (Eval (f x)))  -- Instead of f (f x)
```

Now we can pass `Foo'` to `Twice'` to get what we want:

```
λ> x = Just 'a' :: Twice' (Foo' Int) Bool
λ> :t x
x :: Maybe Char
```

As you can see, now we are passing an unsaturated type constructor (`Foo' Int`) rather than
an unsaturated type family to `Twice'`, so we are good.

Alternatively, we can also turn `Twice'` into a type constructor, and use `Eval` to get
the same result. This would enable us to partially apply `Twice'` as well.

# Final Words

As mentioned in the beginning of this post, this is a simplified approach compared to
the approach introduced in "Thinking with Types". In the latter approach, there is an
additional type parameter for the type constructors, representing the result of the type-level
function. So `Foo'` would be defined like this:

```haskell
data Foo' (x :: Type) (y :: Type) (res :: Type)
```

This is also the approach adopted in the [first-class-families](https://hackage.haskell.org/package/first-class-families)
library.

One obvious benefit of this approach is more clarity and safety, since `Foo'` now explicitly
declares that the result of the type-level function should have kind `Type`. The difference
is more pronounced in the type-level `map` function, where the corresponding type constructor
would look like the following:

```haskell
data MapList :: (a -> Exp b) -> [a] -> Exp [b]
```

where

```haskell
type Exp a = a -> Type
```

First, it correlates nicely with the signature of the term-level `map` function. Second, evaluating
`MapList` with `(a -> Exp b)` and `[a]`, must return `Exp [b]`; it is illegal to return
a different type.

On the other hand, in the
approach I introduced earlier, `MapList` would look like:

```haskell
data MapList :: (a -> Type) -> [a] -> Type
```

which works, but is much less desirable.

Also, explicitly declaring the kind of the result _may_ (I don't know) improve error messages in some cases. I'll leave
it to the reader to determine whether there are further benefits.

Type-level programming in Haskell is generally considered a double-edged sword. It can greatly improve
type safety of your code and solve certain problems beautifully, but at this time, Haskell supports
nowhere near the full capability of System Fω, there are plenty of gotchas, error messages can sometimes be
unreadable, and on top of all that, the learning curve is pretty steep. So, "do it in moderation" seems to be
pretty sound advice.
