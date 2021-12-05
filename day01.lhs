---
title: "AoC Day 1: Sonar Sweep"
author: Jean-Baptiste Mazon
date: 2021-12-01T23:29:41-01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Sweeping all point out of the code
image: aoc-haskell.jpeg
---

[Advent of Code][aoc] is back for a new season!  We're sweeping the
ocean floor with our elven submarine's sonar, surveying zones of
positive slope.

[aoc]: https://adventofcode.com/

Seems like a perfect case to have fun with Haskell's point-free
notation.  Let's have a few imports.

> import Control.Arrow
> import Data.List

Given two lists `as` and `bs`, we can easily compute the difference
between them by zipping:

``` Haskell
\as bs -> zipWith (-) bs as
```

Now in this case, the two lists we want to compare are related: one is
simply the tail of the other.

``` Haskell
\as -> zipWith (-) (tail as) as
```

Now that `as` identifier is repeated, which is another word for
ugly^[In the context of this post.].  But how could we simplify it
out?

The hack^[And I'm not using the word lightly.] is to make use of the
Prelude's provided default Monad instance for function application.
Its specialized signature would look as such:

``` Haskell
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

-- substituting ((->) x) for m:
instance Monad ((->) x) where
  (>>=) :: (x -> a) -> (a -> (x -> b)) -> (x -> b)
```

This is made to look more complicated than it is by the mixing of
prefix and infix notations for the `(->)` type operator, but we can
ask GHCi for an expanded signature to clarify:

``` { .repl }
λ> :t (>>=) @((->) [Int])
(>>=) @((->) [Int])
  :: ([Int] -> a) -> (a -> [Int] -> b) -> [Int] -> b
```

Now to keep things simple, `a` and `b` are going to be `[Int]` as
well, yielding:

``` Haskell
(>>=) :: ([Int] -> [Int]) -> ([Int] -> [Int] -> [Int]) -> [Int] -> [Int]
```

The two `[Int]`s at the end are the function we're trying to get: the
one that takes a stream of integers as an input and returns the
pairwise differences as an output.  It's going to be constructed from
two arguments: the left one that's the conversion from the base list
to the derived one, in our case `tail`, and the right one that's the
combination function, in our case `zipWith (-)`.

Sure enough:

``` Haskell
tail >>= zipWith (-) :: [Int] -> [Int]
```

We can then plug it into an interactive-mode pipeline and get the
expected result back:

``` Haskell
main = interact $ show . length . filter (> 0) . (zipWith (-) =<< tail) . map read . lines
```

I used the `=<<` flipped version of the operator to keep the flows
going in the same direction overall, it's confusing enough as is.

Now for part 2, we still have to perform the filtering and counting,
but this time on a sliding window of 3 consecutive depth measurements.
A perfect opportunity for more flow programming!

Now there exists a `zipWith3` function that would take three lists,
possibly related, and perform some requested actions.  Unfortunately,
there isn't a three-way sum function to plug it in without requiring
abusive point-freeness.

What keeps things simple, on the other hand, is to go directly for
summing over a generic list, that will just happen to always have
length three.  We can pipe components as such:

> part1 = length . filter (> 0) . (zipWith (-) =<< tail)
>
> part2 =
>   tails      >>>  -- the list of tails of the input
>   take 3     >>>  -- restricted to the first three
>   transpose  >>>  -- 3 lists of depths -> list of [3 depths]
>   map sum    >>>  -- sum per window
>   part1           -- and count as before

This is fine, and apart from the chosen piping direction, is how I got
my second star.

But wait!  If I forget my answers and want to solve both parts again,
I have `part1` mentioned twice in my code!  This can't do!

Let's first isolate the computation of part 2 proper:

> part2proper = map sum . transpose . take 3 . tails

Then we'd like to solve it “elegantly” by using the same hack as for
part 1.  But it doesn't appear to apply here, where it's the function
that's duplicated instead of its input.  Or could it?

``` Haskell
twoParts :: [Int] -> (Int,Int)
twoParts = (part1 &&& part1 . part2proper)
         = (&&&) part1 (part1 . part2proper)
         = flip (&&&) (part1 . part2proper) part1
         = (flip (&&&) =<< (. part2proper)) part1
```

But wait!

1. Why am I flipping `&&&` for?  I can remember the part 2 answer
    comes before part 1's, right?
2. Why am I defining anything other than `main`?  Defining a function
   and then calling is two uses of an identifier; such a waste…

So cleaning it all up:

> main = interact $ show . ((&&&) =<< (. map sum . transpose . take 3 . tails))
>                          (length . filter (> 0) . (zipWith (-) =<< tail)) .
>                   map read . lines

Who doesn't like compact code?  Point-free to the point of
pointlessness.  Or to spin it in a more positive light: demonstrating
the true power of first-class functions.

This concludes today's solution.  Can't wait for day 2.
