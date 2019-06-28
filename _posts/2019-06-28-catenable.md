---
title: Efficient Concatenation and Inspection
date: 2019-06-28
---

Functional programmers know all too well that left-associated concatenation of cons-lists can
be a source of inefficiency. For example, if we perform a left-associated concatenation of _n_
singleton cons-lists, it would take _O(n<sup>2</sup>)_ to force the entire result list, and
_O(n)_ just to force the first element. To illustrate the problem, recall that
`head` and `++` are defined as

```haskell
head :: [a] -> a
head (x:_) = x
head [] = error "empty list"

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys  -- (++) has a higher precedence than (:)
```

Therefore

```
  head ((([1] ++ [2]) ++ [3]) ++ [4])
= head (((1 : ([] ++ [2])) ++ [3]) ++ [4])
= head ((1 : (([] ++ [2]) ++ [3])) ++ [4])
= head (1 : ((([] ++ [2]) ++ [3]) ++ [4]))
= 1
```

This shows that it takes 4 steps to force the first element of the left-associated
concatenation of 4 singleton lists. To force the next element, we'd need to
continue to force the head of `((([] ++ [2]) ++ [3]) ++ [4])`. It follows that forcing the
entire list takes _O(n<sup>2</sup>)_.

One might be wondering, why not use right-associated concatenation instead, which gives
_O(1)_ access to the first element and _O(n)_ access to the whole list? Because
left-associated concatenation is not always avoidable. If you have a list of lists to
concatenate, you'd obviously choose to use right-associated concatenation. But any time
you concatenate two lists, the first list may be the result of another concatenation, and
that concatenation may have been done somewhere else.

We obviously would like to bring down the cost of left-associated concatenation to _O(n)_. As to
accessing the first element, it is not possible to do better than _O(n)_ in the worst case, since
in a left-associated concatenation, the first element of the result is
in the deepest layer of the expression tree which has _O(n)_ layers. However, it is desirable if accessing the first
element takes _O(1)_ _amortized_ time. This is already the case with regular
list concatenation: although accessing the head of `(([1] ++ [2]) ++ [3]) ++ [4]` takes
4 steps, it also takes as many steps to construct this expression. The cost of the former can thus
be amortized to _O(1)_. And once we reduce the above expression to `1 : ((([] ++ [2]) ++ [3]) ++ [4])`,
accessing the first element again takes constant time. In bringing down the cost of concatenation, it is
desirable to avoid compromising the performance of _inspecting_ the front of the list.

To this end, two approaches are worth discussing: difference lists, and catenable lists. As we shall see,
difference lists support efficient concatenation, but not efficient inspection. Inspecting the head of
a difference list is an _O(n)_ operation. Catenable lists, on the other hand, supports both
efficient concatenation and efficient inspection. Catenable lists are first presented in the
famous book, [_Purely Functional Data Structures_](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf).

The name "catenable list" is slightly misleading, especially when introduced together
with difference lists, because catenable lists are not just catenable, but also
inspectable, and being inspectable is an important distinction against difference lists. The reason it is
called "catenable list" is probably because lists are by default inspectable. I would probably
call it "catspectable list". Anyway, I'll stick to the standard name.

Both approaches can be generalized to structures other than lists, such as binary trees and monads, which will also be discussed
in this post.

# Difference Lists

Difference lists, often referred to as `DList`, is a fairly well known technique for fast
list concatenation. There's a good number of articles introducing difference lists, so I won't go
into the details. From a high level, difference lists can be understood in a number of
ways:

1. Difference lists effectively turn list concatenation into function composition, and empty list into
   the identity function. Functions can be composed in constant time.
2. Difference lists delay the concatenation by storing the to-be-concatenated lists as
   functions, and only perform the actual concatenation at the end, when a difference list is converted
   to a regular list.
3. Difference lists build up right-associated concatenation expressions, rather than actually concatenating
   the lists.
4. Difference lists is an example of continuation-passing style (CPS). It turns a regular list, `[a]`,
   into a continuation, `[a] -> [a]`. At the term level, a regular list `xs` is turned into
   a continuation, `(xs ++)`. A continuation can be applied to the empty list to recover the
   regular list.

Choose whichever one (or ones) that works the best for you.

## An Issue with Difference Lists

Difference lists are extremely useful, but it suffers from an issue hinted earlier: a
difference list built up via left-associated function composition isn't efficient if we need to repeatedly inspect
the front of the list. For instance, consider the following difference list:

```
((([1] ++) . ([2] ++)) . ([3] ++)) . ([4] ++)
```

To inspect the head of this difference list, we need to apply this function to the empty list, which gives
us a regular list, and then reduce it to weak head normal form, i.e., `1 : _`. This takes _O(n)_ steps. The problem
is that if we need to inspect the head again, we'd have to repeat these steps, which takes another _O(n)_ steps.
Of course, if this is the final difference list we want (i.e., we are done with concatenating to it), we can convert it into
a regular list, and then inspecting the head repeatedly wouldn't be a problem. But it can be a problem if we alternate between
concatenation and inspection.

Here's an extreme example:

```haskell
import qualified Data.DList as D
import Data.List (foldl')

longList :: [Int]
longList = [2..1000000]

f :: Int -> [Int]
f n = take n . D.toList . foldl' g (D.singleton 1) $ map D.singleton longList
  where
    g xs ys
      | D.head xs == 1 = xs <> ys
      | otherwise = ys <> xs  -- unreachable
```

`f n` concatenates a million singleton difference lists, and take
the first `n` elements of the result. At each step, it inspects the head of the current result, and if it is 1,
it appends the next singleton list to the result (`xs <> ys`), otherwise it prepends
the singleton list to the result (`ys <> xs`). This has very poor performance
even for n = 1.

This is of course a silly example, because the head of the result is always 1, and we know that,
so there's no reason to keep checking it for a million times. It's meant to be
an extreme example. In practice it may not be as bad as this, but whenever you inspect the same head more than once,
you are paying a price due to repeating the same steps.

Granted, this is quite unlikely to cause a real problem in practice, because for it to be
a problem, you'd have to repeatedly inspect the front of a left-associated difference list
many times, which is often possible to avoid. After all, if a
difference list remains left-associated, the front of the list will remain the same, and
there's probably no need to repeatedly inspect it. Therefore difference lists remain extremely
useful in practice, as long as you don't do silly things with it like the example above.
That being said, it is possible to overcome this issue using catenable lists.

# Catenable Lists

The reason difference lists are inefficient for repeated inspection is because a difference list is
basically a function, which needs to be applied to an argument (empty list) to get a value, before it can be
inspected. And the reason to convert each list to a function is because we want to delay the actual
concatenation. This may make one wonder: why even bother converting the lists to functions?
If our goal is to delay the concatenation, why do we need to do anything _at all_ to the lists when
concatenating them? Can't we just store the lists in some data structure, and concatenate them only when needed?

If this is what you are thinking, congratulations, you are spot on. Let's try to implement this idea
and see how it goes. We'll use the following `CList` data type:

```haskell
data CList a = Cnil | Ccons a [CList a]
```

A non-empty `CList a` has a head element `a`, and a tail. Unlike regular lists, the tail is
not a `CList a` but a `[CList a]`, containing a number of to-be-concatenated `CList`s. Let's try to
implement the append operator, `<+>`, as well as functions that convert a `CList` to and from a regular
list.

```haskell
data CList a = Cnil | Ccons a [CList a]

(<+>) :: CList a -> CList a -> CList a
Cnil <+> ys = ys
Ccons x xss <+> ys = Ccons x (snoc xss ys)

toRegularList :: CList a -> [a]
toRegularList Cnil = []
toRegularList (Ccons x xss) = x : concatMap toRegularList xss

fromRegularList :: [a] -> CList a
fromRegularList [] = Cnil
fromRegularList [x] = Ccons x []
fromRegularList (x:xs) = Ccons x [fromRegularList xs]

cHead :: CList a -> a
cHead Cnil = error "cHead: empty list"
cHead (Ccons x _) = x

cTake :: Int -> CList a -> CList a
cTake n = fromRegularList . take n . toRegularList
```

where `snoc` is defined as

```haskell
snoc xs x = xs ++ [x]
```

and for now, pretend that `snoc` is _O(1)_ (it is in fact _O(n)_ for cons-lists).

As we can see, the `<+>` operator simply appends `ys`, the second `CList`, to the end of
`xss`, the tail of the first `CList`. Since we are assuming `snoc` is _O(1)_, it follows that `<+>` is an
_O(1)_ operation. It is easy to see that both `toRegularList` and `fromRegularList` take
_O(n)_, and both functions allow the result to be consumed lazily (since they both use guarded recursion).

So everything appears to be good, except that `snoc` is actually an _O(n)_ operation for cons-lists.
Thus cons-list is not a good choice for the tail of `CList`s. We can't simply use
snoc-lists either, because in order to convert `CList a` to `[a]` and access the front of
`[a]` efficiently, the sequence type obviously needs to support efficient access to its front
elements. Therefore, we need a sequence type that supports efficient `snoc` and `uncons`.

If this sounds like a queue, it indeed is. There exist purely functional
queues that offer _O(1)_ queue operations (even in the worst-case). Details
can be found in _Purely Functional Data Structures_. Another good candidate
is the [`Seq`](http://hackage.haskell.org/package/containers/docs/Data-Sequence.html) data type, which is backed by finger trees.

If we run the same example as the one that shows the inefficiency of `DList`, replacing
`DList` with the following `CList` type:

```haskell
data CList a = Cnil | Ccons a (Seq (CList a))
```

it should be much faster thanks to `CList` allowing efficient repeated inspections.

Although `CList` supports both efficient concatenation and efficient inspection,
I personally wouldn't consider it a terribly useful thing in practice for the following reasons:

- As mentioned earlier, I think `DList` is almost always sufficient. As long as you don't run into pathological
use cases like the one shown above, `DList` should be faster than `CList` due to lower overhead. And if you do,
it is often possible to rewrite it in a more efficient way.
- Even if you do have a use case that relies on both concatenation and inspection being fast, you can simply use `Seq`.
Asymptotically, `Seq` is slightly slower than `CList` for concatenation, requiring
_O(log(min(n<sub>1</sub>,n<sub>2</sub>)))_, as opposed to _O(1)_ for an optimal
implementation of `CList`, but it should be hardly noticeable in practice. `Seq` also
supports other efficient operations not supported by `CList`, such as _O(1)_
`unsnoc`. Furthermore, `Seq` is from the `containers` package and the
implementation is highly optimized.

I'm not aware of a catenable list implementation on Hackage. Even if there is one, it certainly has
much less adoption compared to `DList` and `Seq`.

The real value of `CList` lies in the observation that the same idea can be extended to
speed up a variety of data types and operations, such as binary tree concatenation and
monadic bind. In the next section I'll discuss generalizing `CList` to binary trees.

On a side note, it is worth mentioning that a regular list may have multiple corresponding
`CList`s with different structures. For example, converting [1,2,3] to
`CList` could result in either of the following two `CList`s:

```haskell
Ccons 1 [ Ccons 2 [ Ccons 3 [] ] ]
Ccons 1 [ Ccons 2 [], Ccons 3 [] ]
```

The `fromRegularList` function returns the former, but it's an arbitrary choice.
The choice does not affect performance, at least asymptotically.

# Generalizing to Binary Trees

Suppose we have the following binary tree data type, and a function to append
a tree onto another tree, which replaces all `Null` nodes in the first tree
with the second tree:

```Haskell
data Tree a = Null | Fork (Tree a) a (Tree a)

(××) :: Tree a -> Tree a -> Tree a
Null ×× t = t
Fork l x r ×× t = Fork (l ×× t) x (r ×× t)
```

Since `××` traverses the first tree, it has exactly the same problem as `++` when
left-associated.

Both `DList` and `CList` can be generalized to binary trees. `DList` can be easily generalized
to any monoid:

```haskell
type DMonoid a = a -> a

fromMonoid :: Semigroup a => a -> DMonoid a
fromMonoid = (<>)

toMonoid :: Monoid a => DMonoid a -> a
toMonoid = ($ mempty)

append :: Semigroup a => DMonoid a -> DMonoid a -> DMonoid a
append = (.)

concat :: Semigroup a => [DMonoid a] -> DMonoid a
concat = foldr append id
```

It is also easy to generalize `CList` to binary trees:

```haskell
data CTree a = Cnull | Cfork [CTree a] a [CTree a]
```

The implementation of the append operator `<×>`, and the functions
`toRegularTree` and `fromRegularTree`, are left as exercises.

# Monadic Binds

Both `DList` and `CList` can also be generalized to speed up left-associated monadic binds.
The counterpart of `DList` for monadic binds is [Codensity](http://hackage.haskell.org/package/kan-extensions/docs/Control-Monad-Codensity.html), and the counterpart of
`CList` is type-aligned sequences, introduced
in [_Reflection without Remorse_](http://okmij.org/ftp/Haskell/zseq.pdf). I won't go
into these since this post is already getting quite long.
With a good grasp of `DList` and `CList`, it is not too hard to understand Codensity
and type-aligned sequences by making analogies.

# Further Reading

- [_Purely Functional Data Structures_](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)
- [_Reflection without Remorse_](http://okmij.org/ftp/Haskell/zseq.pdf)

# Exercises

1. Implement `<×>`, `toRegularTree` and `fromRegularTree` for `CTree`.
2. We generalized `DList` to `DMonoid` for any monoid. Can a similar generalization
   be made to `CList`?
3. Modify `DList` and `CList` for non-empty lists.
