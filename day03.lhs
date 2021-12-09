---
title: "AoC Day 3: Binary Diagnostic"
author: Jean-Baptiste Mazon
date: 2021-12-03T18:43:05+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Sweeping all point out of the code
image: aoc-haskell.jpeg
---

Today's AoC challenge, [“Binary Diagnostic”][aoc], asks us for a few
statistics on a list of binary numbers.  This post is a [literate
Haskell program][gh]^[<del>As soon as I get to publishing it, of
course.</del> Done at last.  Whew.], so let's get a few imports out of
the way.

[aoc]: https://adventofcode.com/2021/day/3
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day03.lhs

> import Control.Arrow ((&&&))
> import Control.Monad (guard)
> import Data.Char     (digitToInt)
> import Data.Function (on)
> import Data.List     (foldl',group,sort,transpose)
> import Data.Maybe    (mapMaybe)

We'll be dealing with a number's binary representation a lot.  Let's
define a type for that from the start.

> type BinNum = [Bool]

The puzzle input is a list of such numbers represented in ASCII.  The
decoding function is simple and straightforward.

> decode :: String -> [BinNum]
> decode = (map . map) (toEnum . digitToInt) . lines

The gamma rate is the most common number bit, sequenced per position.
So let's determine what the most common bit is within a group.

> mostCommon :: [Bool] -> Bool
> mostCommon = decide . map length . group . sort . (False :) . (True :)
>   where decide [a,b] = a <= b

The internal function `decide` is one of those cases where simplifying
boolean expressions can go too far.  The core idea is to return the
most common bit's value given a
$\left(\left|False\right|,\left|True\right|\right)$ pair, the
population counts for both bit values.  The result should be `False`
if $a\gt b$ and `True` if $a\lt b$.  Which simplifies nicely^[But
confusingly.]  to just $a\lt b$.

I'm force-including two samples to ensure the resulting grouped sorted
list has `False` and `True` where I expect them, even when absent from
the original.

Now the gamma rate can be computed by applying that `mostCommon`
operation bit by bit.

> part1 :: [BinNum] -> Int
> part1 = uncurry (*~) . (id &&& map not) . map mostCommon . transpose


This uses a simple helper to multiply two numbers after converting
them from binary representation.

> (*~) :: BinNum -> BinNum -> Int
> (*~) = (*) `on` foldl' (\a b -> 2*a + fromEnum b) 0

So far, so good.

For part 2, instead of picking a bit by index, we'll *filter*.  We
really need to trawl through the entire set to actually determine the
partition criterion, so point-free style doesn't seem so appealing
right now.  Instead we'll alternate filtering and selecting until we
narrowed it down to a single number.

> select :: (Bool -> Bool) -> [BinNum] -> BinNum
> select f = go [] where
>   go acc [bs] = reverse acc ++ bs
>   go acc bss = go (b : acc) $ mapMaybe g bss
>     where b = f (mostCommon (map head bss))
>           g (h:t) = t <$ guard (h == b)

What's that `f` argument?  It's a hook to allow inverting the answer
when switching bewteen oxygen generator and CO<sub>2</sub> scrubber.
We can then apply both bit criteria and merge to a puzzle answer.

> part2 :: [BinNum] -> Int
> part2 = uncurry (*~) . (select id &&& select not)


A main wrapper to solve it all:

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . decode

This concludes today's solution.  See you tomorrow!
