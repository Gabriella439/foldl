# `foldl` v1.4.0

Use this `foldl` library when you want to compute multiple folds over a
collection in one pass over the data without space leaks.

For example, suppose that you want to simultaneously compute the sum of the list
and the length of the list.  Many Haskell beginners might write something like
this:

```haskell
sumAndLength :: Num a => [a] -> (a, Int)
sumAndLength xs = (sum xs, length xs)

```

However, this solution will leak space because it goes over the list in two
passes.  If you demand the result of `sum` the Haskell runtime will materialize
the entire list.  However, the runtime cannot garbage collect the list because
the list is still required for the call to `length`.

Usually people work around this by hand-writing a strict left fold that looks
something like this:

```haskell
{-# LANGUAGE BangPatterns #-}

import Data.List (foldl')

sumAndLength :: Num a => [a] -> (a, Int)
sumAndLength xs = foldl' step (0, 0) xs
  where
    step (x, y) n = (x + n, y + 1)
```

That now goes over the list in one pass, but will still leak space because the
tuple is not strict in both fields!  You have to define a strict `Pair` type to
fix this:

```haskell
{-# LANGUAGE BangPatterns #-}

import Data.List (foldl')

data Pair a b = Pair !a !b

sumAndLength :: Num a => [a] -> (a, Int)
sumAndLength xs = done (foldl' step (Pair 0 0) xs)
  where
    step (Pair x y) n = Pair (x + n) (y + 1)

    done (Pair x y) = (x, y)
```


However, this is not satisfactory because you have to reimplement the guts of
every fold that you care about and also define a custom strict data type for
your fold.  Hand-writing the step function, accumulator, and strict data type
for every fold that you want to use gets tedious fast.  For example,
implementing something like reservoir sampling over and over is very error
prone.

What if you just stored the step function and accumulator for each individual
fold and let some high-level library do the combining for you?  That's exactly
what this library does!  Using this library you can instead write:

```haskell
import qualified Control.Foldl as Fold

sumAndLength :: Num a => [a] -> (a, Int)
sumAndLength xs = Fold.fold ((,) <$> Fold.sum <*> Fold.length) xs

-- or, more concisely:
sumAndLength = Fold.fold ((,) <$> Fold.sum <*> Fold.length)
```

To see how this works, the `Fold.sum` value is just a datatype storing the step
function and the starting state (and a final extraction function):

```haskell
sum :: Num a => Fold a a
sum = Fold (+) 0 id
```

Same thing for the `Fold.length` value:

```haskell
length :: Fold a Int
length = Fold (\n _ -> n + 1) 0 id
```

... and the `Applicative` operators combine them into a new datatype storing
the composite step function and starting state:

```haskell
(,) <$> Fold.sum <*> Fold.length = Fold step (Pair 0 0) done
  where
    step (Pair x y) = Pair (x + n) (y + 1)

    done (Pair x y) = (x, y)
```

... and then `fold` just transforms that to a strict left fold:

```haskell
fold (Fold step begin done) = done (foldl' step begin)
```

Since we preserve the step function and accumulator, we can use the `Fold` type to
fold things other than pure collections.  For example, we can fold a `Producer`
from `pipes` using the same `Fold`:

```haskell
Fold.purely Pipes.Prelude.fold ((,) <$> sum <*> length)
    :: (Monad m, Num a) => Producer a m () -> m (a, Int)
```

To learn more about this library, read the documentation in
[the main `Control.Foldl` module](http://hackage.haskell.org/package/foldl/docs/Control-Foldl.html).

## Quick start

Install [the `stack` tool](http://haskellstack.org/) and then run:

```bash
$ stack setup
$ stack ghci foldl
Prelude> import qualified Control.Foldl as Fold
Prelude Fold> Fold.fold ((,) <$> Fold.sum <*> Fold.length) [1..1000000]
(500000500000,1000000)
```

## How to contribute

Contribute a pull request if you have a `Fold` that you believe other people
would find useful.

## Development Status

[![Build Status](https://travis-ci.org/Gabriel439/Haskell-Foldl-Library.png)](https://travis-ci.org/Gabriel439/Haskell-Foldl-Library)

The `foldl` library is pretty stable at this point.  I don't expect there to be
breaking changes to the API from this point forward unless people discover new
bugs.

## License (BSD 3-clause)

Copyright (c) 2016 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
