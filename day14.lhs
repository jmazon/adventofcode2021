---
title: "AoC Day 14: Extended Polymerization"
author: Jean-Baptiste Mazon
date: 2021-12-14T10:48:21+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Exponential growth strikes again
image: aoc-haskell.jpeg
---

For day 14 of Advent of Code, [“Extended Polymerization”][aoc], we are
to repeatedly double a string by inserting a specific character
between each pair according to a given ruleset.  This post is your
usual [literate Haskell][gh], so let's import what needs be and get
started.

[aoc]: https://adventofcode.com/2021/day/14
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day14.lhs

> import Control.Arrow ((&&&))
> import Data.List     (group,sort)
> import Data.Map.Strict hiding (map,lookup)

The rule format is a simple “between such pair of characters, insert
this new character”.

> type Rule = ((Char,Char),Char)

For part 1, we are to double the string 10 times.  So let's do just
that.

> part1 :: String -> [Rule] -> Int
> part1 polymer rules = (format . tally . (!! 10) . iterate step) polymer
>   where
>     step (a:bs@(b:_)) | Just c <- lookup (a,b) rules = a : c : step bs
>     step [x] = [x]
>     tally = map length . group . sort

The `step` function pattern-matches the string head, looks up the
pair, replaces and recurses.  The `tally` function lazily delegates to
list sorting and grouping operations, which is slightly suboptimal,
but so much easier to write out.^[Especially in Haskell.]

The rest of the checksumming process will be shared with part 2, so
I'm heaving it up to a top-level function.

> format :: Foldable f => f Int -> Int
> format = uncurry (-) . (maximum &&& minimum)

For part 2, the question is the same, the difference is we are to
iterate 40 times.

It's not a trivial difference.  My starting polymer template is $20+1$
characters long, so whereas iterating 10 times resulted in a $20\times
2^{10}+1\approx 20\mathrm{Ki}$ character long string, iterating
40 times brings that figure to $20\times 2^{40}+1\approx
20\mathrm{Ti}$ characters, which definitely won't fit in my RAM.

Luckily, we're not asked for the resulting polymer, only statistics
about its constituting elements.  So let's reason directly on those.

Iteration is based on the pairs' structure, so that's what we're going
to keep.

> part2 :: String -> [Rule] -> Int
> part2 polymer rules =

To convert a polymer structure to its list of constituent pairs, I'll
use the same reader monad trick I [expanded on
day 1](day01.html).^[It's still as unnecessary as on day 1, and I'm
still not recommending it for general use.  This is Advent of Code,
I'm indulging a little fun.]  And feed it to a map.

>   let
>     pairs = fromListWith (+) . flip zip (repeat 1) . (flip zip =<< tail)

To convert back to a simple tally, I'll just add both components of a
map's key.  A slight adjustment is necessary for the endpoints, as
they're part of one fewer pairs than the others.

>     unPairs = fmap (`div` 2) .
>               adjust (+1) (head polymer) .
>               adjust (+1) (last polymer) .
>               uncurry (unionWith (+)) .
>               (mapKeysWith (+) fst &&& mapKeysWith (+) snd)

The adjusted stepping function then performs the same lookup as in
part 1, just limited to once per pair type.

>     polymerStep = unionsWith (+) . map pairStep . assocs
>     pairStep ((a,b),n) | Just c <- lookup (a,b) rules
>       = fromListWith (+) [((a,c),n),((c,b),n)]

Bringing it all together:

>   in
>     (format . unPairs . (!! 40) . iterate polymerStep . pairs) polymer

It's worth reflecting a bit at this point.  We're evidently facing a
linear transformation [again](day06.html), so why am I not building up
its transformation matrix and applying exponentiation by squaring?
The answer is that while it would work, we're not iterating enough
times for it to be a problem.  What made the problem intractable using
the brute force approach was to keep the pairs' ordering, but once
we're past that, our (kinda naïve) step function is still $O(rules^2)$,
which is small, iterated 40 times, which is still small.  I'll get my
matrices back out when the iteration count reaches the millions.

The wrapping doesn't warrant too much explanation.

> parse :: String -> (String,[Rule])
> parse (lines -> polymer:"":rules) = (polymer,readRule <$> rules)
>   where readRule (words -> [[a,b],"->",[c]]) = ((a,b),c)
> 
> main :: IO ()
> main = interact $ show . (uncurry part1 &&& uncurry part2) . parse

This concludes today's solution.  See you tomorrow!
