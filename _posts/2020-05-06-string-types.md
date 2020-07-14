---
title: Eat Haskell String Types for Breakfast
date: 2020-05-06
---

This blog post summarizes, with bite-size bullet points, some knowledge on Haskell string types that I think is important to
recognize when writing Haskell. The intended audience include inexperienced Haskell programmers, and
experienced Haskell programmers who feel like refreshing their memory. The bullet list hopefully makes things reasonably
easy to digest, and helps you eat Haskell string types for breakfast.

Haskell has five commonly used string types: `String`, strict and lazy `Text`, and strict and lazy `ByteString`[^1]. Some recommand
against calling `ByteString` a string type, The following is
roughly in the order of `String`, `Text`, `ByteString`.

- `String` is, in some sense, the "official" string type, since it is the only string type in `base` (and of course,
the only one in `Prelude`).

- `String` is unfortunately highly inefficient, because it is quite simply a linked list (specifically, a cons-list)
of (boxed) `Char`s, each of which represents a unicode code point.
This means each `Char` in a `String` is in a cons-cell with a `(:)` tag, a pointer to the `Char` on the heap, and a pointer
to the next cons-cell.

  How many bytes a `String` uses per character on a 64-bit machine is left as an exercise, but it is certainly a _lot_ more than
  what is theoretically required to encode a unicode code point: between 1 and 4 bytes. It also suffers
  from poor locality for obvious reasons.

- `String` is convenient when dealing with certain parts of `base`, for instance `show` and `displayException`, as well as the
  [_filepath_](https://hackage.haskell.org/package/filepath) and [_directory_](https://hackage.haskell.org/package/directory) libraries,
  but there are few other legitimate use cases of `String`. For many `base` functions
  that work on `String`s, such as `reverse`, `takeWhile` and `isInfixOf`, there are corresponding versions in `Data.Text`.

- `Text`, like `String`, also represents a sequence of unicode characters, but is much more tightly packed. Thus it can
often be a drop-in replacement for `String`.

- Strict `Text` is a (strict) [array](http://hackage.haskell.org/package/text/docs/Data-Text-Array.html) of bytes. Lazy `Text`
is a (lazy) cons-list of strict `Text`s, where each strict `Text` is called a _chunk_, and the lazy cons-list is also
known as the _spine_. Internally, strict `Text` is UTF-16 encoded, so each
character occupies either 2 or 4 bytes (plus some constant overhead for each `Text`).
  - `Text` uses UTF-16 rather than the more popular UTF-8, because although UTF-8 is more space-efficient for ASCII
  characters, its arithmetic is relatively more complex, and converting bytes to/from `Char`s is more expensive.
  Furthermore, the space saving of UTF-8 is insignificant for small `Text` values.
  See [_Text/UTF-8: Aftermath_](https://jaspervdj.be/posts/2011-08-19-text-utf8-the-aftermath.html) for more details.
  - The specific encoding `Text` uses internally, whether UTF-8 or UTF-16, does not usually affect the way most users use the library.
  Unless you are using stuff in the `Internal` modules, the unit you deal with is `Char`.
  - If you prefer a `Text` type that internally uses UTF-8 (for example, if you have a lot of interaction with UTF-8 encoded data),
  consider [text-short](https://hackage.haskell.org/package/text-short)
  or [core-text](https://hackage.haskell.org/package/core-text).

- If you are familiar with Java, it might be helpful to think of lazy `Text` as `LinkedList<ArrayList<Byte>>` (except that the
`LinkedList` is a lazy cons-list), where each `ArrayList<Byte>` is a chunk, and the `LinkedList` is the spine.
This gets the benefits of both `LinkedList` and `ArrayList`: it supports efficient _O(1)_ concatenation[^2] because `LinkedList`
concatenation is _O(1)_; it also is more space efficient and has better locality
than a flat `LinkedList<Byte>`, because `ArrayList` is stored contiguously in memory
and there's no pointer from a node to the next.

  - Some have argued that it is better to use a _strict_ list/rope of chunks for lazy `Text`, which would be more comparable to
  Java's `LinkedList<ArrayList<Byte>>`. The advantage is that this can potentially unify strict and lazy `Text` into
  a single type, because it is both strict, and efficient in concatenation (and similar operations) like lazy
  `Text` is. The disadvantage, however, is that there doesn't exist (at least not yet, as far as I know) an efficient
  implementation of `LinkedList` (or any data structure) in Haskell whose performance is competitive in terms of constant factors
  to raw byte arrays (which is what backs strict `Text`), and it may well be impossible.

    Besides, there are legitimate use cases, though perhaps rare, that require the `Text` to be lazy, for instance
    when you want to perform lazy IO without bothering to use conduit or pipes (you most likely should, though).

- Many operations on `Text` are _subject to fusion_. The goal of fusion is to avoid materializing intermediate results when performing
  a chain of operations. The core idea of fusion is similar to improving list concatenation
  using difference lists, and improving a chain of `fmap` using Coyoneda, and can be summarized as: _delay, delay, delay_. Delaying the
  construction of a data structure is key for avoiding materializing the intermediate results. One way to
  delay an operation on a data structure is to turn the data structure into an arrow (e.g., function or Kleisli arrow), turn the
  operation into composition (e.g., function composition or Kleisli composition), and finally, apply the arrow to some argument to get the result only when needed.

  As an example, there are at least two ways to turn a Haskell list into functions, and they are related in an interesting way:

  1. Representing the list inductively (a.k.a. build/foldr fusion system):
  ```haskell
  newtype List a = List { build :: forall b. (a -> b -> b) -> b -> b }
  ```
  This represents a list in terms of fold. It is equivalent to `Mu (ListF a)` where `Mu f` is the least fixed
  point of `f`, and `ListF a` is the base functor of `[a]`, i.e.,
  ```haskell
  data ListF a b = Cons a b | Nil
  ```

  2. Representing the list coinductively (a.k.a. stream fusion):
  ```haskell
  data List a = forall s. List (s -> Step a s) s
  data Step a s = Yield a s | Done
  ```
  This represents a list in terms of unfold. It is equivalent to `Nu (ListF a)` where `Nu f` is the greatest fixed point
  of `f`.

  So these two approaches are dual of each other. In both cases, a list is represented in a "delayed" form,
  and operations such as `map` simply "remembers" the function
  to be mapped, rather than actually performing the mapping and generating a new list. The pros and cons of the two
  approaches is out of the scope of this blog post, but [this paper](http://fun.cs.tufts.edu/stream-fusion.pdf) has more details
  if you are interested.

  In the case of `Text`, it employs stream fusion. Each eligible operation `op` is turned into `fromStream . opOnStream . toStream`,
  where `opOnStream` is the streaming version of `op`, and a rewrite rule is used to cancel
  `toStream . fromStream` into the identity function.

- `ByteString`, unlike `String` and `Text`, has little to do with `Char` or unicode. It is just a sequence of bytes.
To convert a `ByteString` to/from `Text`, you need to specify which encoding you intend to use, and then
use the conversion functions in `Data.Text.Encoding`.

- Like `Text`, strict `ByteString` is backed by a byte array, and lazy `ByteString` is backed by a cons-list of chunks, each
of which is a strict `ByteString`. One difference is that strict `ByteString` uses a `ForeignPtr Word8` as the
underlying data structure. This makes it directly usable in FFI because it has the same memory layout as
an `unsigned char *` in C (note that a `char` in C is a byte, not a unicode code point).
  - To pass a `ByteString` to a C function, you just need to do two things: (1) make a copy of it (because otherwise the C function may
  mutate it), and (2) append a null (0x00) byte, since C strings are null-terminated. This is exactly what `Data.ByteString.useAsCString` does.

- Functions in `Data.ByteString.Char8` behave as if each byte is a `Char`, and that's why you
don't need to (and don't get to) specify an encoding to use any function in this module. It works well if your `ByteString` contains ASCII
characters only, but probably not otherwise; for example, `Char8.unpack (Char8.pack "😈😈😈") /= "😈😈😈"`. Use the `Char8` module
if you are certain that you are dealing with ASCII characters, or if otherwise you know what you are doing.

- When using `Data.Text.IO.hGetLine` and `Data.Text.IO.hGetContents`, make sure the `Handle` has the correct encoding. You'd otherwise get
an "invalid argument (invalid byte sequence)" error. Since these methods rely on using the correct encoding, it is often wise to use their counterparts
in `Data.ByteString`. `ByteString`, having no assumption on the encoding, is more suitable for exchanging information across machines. After reading
the `ByteString`s in, you can then decode them using the appropriate decoders and handle errors properly.
  - This is probably why Aeson's `encode` and `decode` functions work with `ByteString` rather than `Text`, even though JSON is supposed
    to be textual and human readable. JSON is often exchanged over the network, and since data are sent or received over the network as
	sequences of bytes, you'd need `ByteString`. Furthermore, although the overwhelming majority of JSON texts are UTF-8 encoded, this is not a guarantee,
	so using `Text` prematurely can be risky.
  - That being said, the Aeson library does provide `Text`-based encoder and decoder in the `Data.Aeson.Text` module.
  - On a side node, Aeson's `encode` function returns a _lazy_ `ByteString`, because the result is assembled piece by piece, and
    lazy `ByteString`, like lazy `Text`, has better asymptotics for operations like concatenation.

- `Data.ByteString.getLine` and `Data.ByteString.hGetLine` both read until EOF or a _newline byte_, i.e., 0x0A. Beware if your data
is not UTF-8 encoded. For example, in UTF-16BE and UTF-16LE, the newline character has two bytes, and is encoded
as 0x00 0x0A and 0x0A 0x00, respectively. Arguably, these two functions should only be exposed from the `Char8` module, because a newline
is a _character_.

- If you have a function that takes a lazy `Text` as input, consider taking instead
a stream that produces `Text` (e.g., `ConduitT i Text m ()` in conduit, or `Producer' Text m ()` in pipes). Because you can easily
convert a lazy `Text` into a stream, but the converse is not true because the stream may be effectful. The same goes for `ByteString`.

- `ByteString` used to employ stream fusion like `Text` does, but currently fusion is not used in `ByteString`, whether
strict or lazy. Here's [the author's explanation](https://github.com/haskell/bytestring/issues/81#issuecomment-244082219)
of why fusion is no longer used.

- The bytestring package also has a `ShortByteString` type. It can be a good option if you need to keep a large number of small `ByteString`s around.

# Further Reading

- Here's a few other blog posts on Haskell string types: [String Types](https://tech.fpcomplete.com/haskell/tutorial/string-types) by FP Complete,
  [Untangling Haskell's Strings](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings) by Monday Morning Haskell, and
  [Haskell String Types](https://www.alexeyshmalko.com/2015/haskell-string-types/) by Alexey Shmalko.
- To learn more about fixed points, recursion schemes, `Mu`, `Nu` and what not, check out the [_recursion-schemes_](https://hackage.haskell.org/package/recursion-schemes)
  and the [_yaya_](https://hackage.haskell.org/package/yaya) library, and [my previous blog post](https://free.cofree.io/2019/08/21/mu-nu/) on this topic.
- To learn more about `ShortByteString`, check out Mark Karpov's blog post, [_Short ByteString and Text_](https://markkarpov.com/post/short-bs-and-text.html).

---

[^1]: I should remark that `ByteString` is technically not a string type, because a string is a sequence of characters, and `ByteString` is a sequence of bytes - it has no notion of characters. So "ByteString" is perhaps not the most fitting name. It is, however, appropriate to discuss `ByteString` together with `String` and `Text`, since the correct choice among these types is often a point of confusion.

[^2]: Concatenation of two lazy `Text`s is not actually _O(1)_ because Haskell's cons-list doesn't support _O(1)_ concatenation.It is _O(c<sub>1</sub>)_ where _c<sub>1</sub>_ is the number of chunks in the first `Text`. It is still a much better asymptotic than concatenating two strict `Text`s, which is _O(n<sub>1</sub> + n<sub>2</sub>)_ where _n<sub>1</sub>_ and _n<sub>2</sub>_ are the lengths of the two `Text`s.
