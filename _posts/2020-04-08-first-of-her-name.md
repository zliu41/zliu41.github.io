---
title: A Haskell Solution to "First of Her Name" (ACM-ICPC World Finals 2019)
date: 2020-04-08
---

Today I'm going to discuss a solution which I implemented in Haskell to a problem in ACM-ICPC World Finals 2019, titled
"First of Her Name". The solution makes use of a sorting technique called "prefix doubling" as seen in suffix array
algorithms, as well as radix sort.

# Problem Description

The description of the problem can be found [here](/assets/resources/icpc2019.pdf) (problem G). The problem
descriptions in ACM-ICPC tend to start with some fancy stories rather than directly getting to the problems, so to save
you some time, here's a much simplified description:

You are given a list of `(Char, Int)` pairs, each of which representing a string. The `Char` component is the first letter
in the string, and the `Int` component is the (1-based) index of the next letter. If the `Int` is zero, then it is the last letter.

For example,

```
[ ('A', 3)                      [ "ANOM"
, ('O', 5)                      , "OM"
, ('N', 2)                      , "NOM"
, ('O', 3)  -- represents -->   , "ONOM"
, ('M', 0)                      , "M"
, ('D', 1)                      , "DANOM"
, ('D', 8)                      , "DIONOM"
, ('I', 4)                      , "IONOM"
]                               ]
```

The input satisfies

1. It is always valid. There's no loop or invalid index or anything like that.
2. There is exactly one single-letter string. In other words, there is exactly one `(Char, Int)` whose second
  component is 0. This also implies that all strings end with the same letter.
3. All strings are unique.

You are then given a list of prefixes. For each prefix, print how many strings start with that prefix. For example,
given `["ON", "D"]`, print `[1, 2]`.

# Naive Solution

A straightforward solution is to sort the input, then perform a binary search for each prefix query. What's the time
complexity? Let _n_ denote the total number of strings. Sorting a list of _n_ values takes _O(n*logn)_ comparisons.
In this particular case, each comparison takes _O(n)_ time because the longest string can have as many as _n_ letters.
This makes the cost of this approach _O(n<sup>2</sup>*logn)_. The problem description informs us that _n_ is up to
10<sup>6</sup>, so this approach can't possibly be accepted.

# Improved Solution Using the Suffix Array Doubling Idea

To improve the performance, a crucial observation is that the input is _not_ an arbitrary list of strings. Rather, for
each string in the input, all its suffixes are also there. The list of strings in the above example input is really
composed of all (unique) suffixes of "DANOM" and "DIONOM" (they happen to be the reverse of "MONAD" and "MONOID" but whatever).

This gives us an out: we can use an idea to sort the input similar to that of suffix arrays, which sorts all suffixes
of a string. The idea is this:

_Because for each string in the list, all its suffixes are also in the list, when we sort the list on prefixes of a certain length (for
instance, on the first two letters), we not only know the rank of each string wrt its first two letters
(for instance, "DANOM" ranks second), but we also know the rank of each of its suffixes wrt the
first two letters (for instance, "ANOM" ranks first, "NOM" ranks sixth, and so on). This information can help us
efficiently sort the list of strings on the first __four__ letters._

This idea is called "prefix doubling" in the suffix array algorithm. In the first pass we sort the
list of strings on the first letter. Using this information, we can then make a second pass and sort the
list of strings on the first two letters; then the first four letters, then the first eight, and so on. Each pass takes
_O(n*logn)_ (because comparison is now _O(1)_), and we need _O(logn)_ passes, so the time complexity of sorting
all strings in the input is reduced to _O(n*log<sup>2</sup>n)_.

The code for this approach can be found in [Simple.hs](https://github.com/zliu41/hs-acm-icpc/blob/master/world-finals/2019/src/FirstOfHerName/Simple.hs). The `rankify` function makes a sorting pass. It has the following type:

```haskell
rankify :: forall a b. Ord a => Array (a, b) -> (Array (Int, b), Bool)
```

Each time `rankify` is called, the prefix length on which the strings are sorted is doubled. It keeps `rankify`ing the strings until all ranks are unique (for example, if the first letters of the strings are all distinct, then only a single pass is needed), which must
eventually happen because all strings in the input are unique.

The type parameter `a` is instantiated to `Char` the first time `rankify` is called, corresponding to sorting the strings
on the first letter. Every other time `rankify` is called, `a` is instantiated to `(Int, Int)`, where the first `Int` is the rank
of the first half of the prefix to be sorted on, and the second `Int` is the rank of the second half.

Take "DANOM" as an example. In the first pass we sort on the first letter, i.e., 'D', and the rank of "DANOM" is 2. Because the ranks are not
all distinct ("DANOM" and "DIONOM" have the same rank; so do "OM" and "ONOM"), we need another pass. In the second pass, since the rank of
'D' is 2 and the rank of 'A' is 1, the value of `a` would be (2, 1) for "DANOM".

# Further Improved Solution Using Radix Sort and Mutable Unboxed Vectors

In the above approach, in each pass we either sort on `Char`, or sort on `(Int, Int)`. Since the input strings only consist of
upper case English letters, we can replace `Char` by `Int8`. And since the length of the input is no more than 10<sup>6</sup>, we
can replace `(Int, Int)` by `(Int32, Int32)`. In both cases, [radix sort](https://en.wikipedia.org/wiki/Radix_sort) can be
used to bring the complexity down to _O(n)_ (radix sort does not do comparison, which is why _O(n)_ is achievable).

A Haskell implementation of radix sort on mutable vectors is available in the
[vector-algorithms](https://hackage.haskell.org/package/vector-algorithms-0.3/docs/Data-Vector-Algorithms-Radix.html) package.
Mutable vectors are possible in Haskell thanks to the ST monad, which uses Rank-2 types to make sure the mutation of a vector is not
observable or accessible outside the ST computation that creates the mutable vector. And since we are dealing with
either `Int8` or `(Int32, Int32)`, we can use unboxed vectors to further improve the performance.

The code for this approach can be found in [RadixSort.hs](https://github.com/zliu41/hs-acm-icpc/blob/master/world-finals/2019/src/FirstOfHerName/RadixSort.hs). The `rankify` function takes a regular, immutable vector `xs`, and `unsafeThaw` it into
a mutable vector, `xs'`. `unsafeThaw` is unsafe in the sense that it simply makes the original vector `xs` mutable. So if
you mutate it, and subsequently use `xs`, you will observe the mutation even though `xs` is supposedly immutable.
But it is safe as long as you no longer use `xs` after mutating `xs'`, which is the case here. Since sorting is now _O(n)_, the
total time complexity is further reduced to _O(n*logn)_.

# Final Thoughts

I'm not aware of any ACM-ICPC contest (regionals or finals) that allows Haskell. In fact I don't think Haskell is suitable for
this kind of programming contests, and I wouldn't use it even if it is allowed. The purpose of these contests is not to
write composable, maintainable and production quality software, but to solve individual problems as quickly as possible, using usually
no more than 150 lines of code for each problem, and it is unlikely that anyone will ever look at or run the code once the contest is over.
Haskell's type system imposes a number of constraints that tend to make writing code somewhat harder in exchange for code
that is safe and easy to reason about (for more on that, watch Runar Bjarnason's talk
[_Constraints Liberate, Liberties Constrain_](https://www.youtube.com/watch?v=GqmsQeSzMdw)), but that is the
exact opposite of what you want in a programming contest.
Nevertheless, trying to solve these problems in Haskell is of great fun for Haskell enthusiasts.
