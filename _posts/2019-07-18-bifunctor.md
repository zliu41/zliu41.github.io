---
title: A Simple Counter Example of Joint Functoriality
date: 2019-07-18
---

A bifunctor is a functor whose source is a product category. In Haskell,
bifunctors have the `bimap` operation:

```haskell
class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
```

The laws for `bimap` are:

```
bimap id id = id
bimap (f . f') (g . g') = bimap f f' . bimap g g'
```

which are nothing but a reiteration of functoriality.

Bartosz Milewski, in [one of his blog posts](https://bartoszmilewski.com/2015/02/03/functoriality/),
mentioned that functoriality in each argument does not imply joint functoriality
(i.e., the above `bimap` laws). However, he didn't provide an example. Here
I'm going to show a simple example that proves

```
  bimap (f . f') (g . g') = bimap f f' . bimap g g'
⤂
  bimap f (g . g') = bimap f g . bimap f g'  Λ
  bimap (f . f') g = bimap f g . bimap f' g
```

In words, `bimap` jointly preserving composition does _not_ follow from preserving
composition in each argument. Note that variables in all three equations above
are universally quantified.

# The Example

The example is in a category with a single object: a set of two elements `{A, B}`, and
two arrows: the function `id`, and the function `const A`. Arrows are composed via
regular function composition. Define `bimap` as

```haskell
bimap f g = if f == id || g == id then id else const A
```

now, let `f = g' = id` and `f' = g = const A`. Then we have

```
bimap (f . f') (g . g') = bimap (const A) (const A) = const A
bimap f g . bimap f' g' = id . id = id
```

Therefore `bimap` does not jointly preserve composition, and so is not a bifunctor.
However, `bimap` does preserve composition in each argument. Suppose we fix `f`:

- If `f = id`:
  ```
  bimap f (g . g') = id = bimap f g . bimap f g'
  ```
- If `f = const A`, and at least one of `g` and `g'` is `id` (without loss of generality, suppose `g = id`):
  ```
  bimap f (g . g') = bimap f g' = bimap f g . bimap f g'
  ```
- If `f = g = g' = const A`:
  ```
  bimap f (g . g') = bimap (const A) (const A) = const A = bimap f g . bimap f g'
  ```

This shows that `bimap` preserves composition in the second argument. By symmetry, it
also preserves composition in the first argument.

# Conclusion

A mapping from a product category to another category is not necessarily a
bifunctor if it only preserves composition in each argument, since this does not
imply preserving composition jointly.
