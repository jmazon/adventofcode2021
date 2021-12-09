---
title: "AoC Day 7: The Treachery of Whales"
author: Jean-Baptiste Mazon
date: 2021-12-07T11:50:54+01:00
updated: 2021-12-09T08:14:13+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Retrocoding
image: aoc-haskell.jpeg
---

[Advent of Code day 7][aoc] is a bit weird, in that I didn't really
code my way to the gold stars.  Still, it accepts a rather simple
coded solution, so let's write it out.  This post is [literate
Haskell][gh], here are some imports.

[aoc]: https://adventofcode.com/2021/day/7
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day07.lhs

> import Control.Arrow
> import Data.List.Split
> import Data.Array

The general idea is to find a position that minimizes the sum of costs
to reach it.  Now cost is very closely related to distance, which is a
convex function, so is a sum of multiple of it, so we can find its
minimum in logarithmic time using (integer) ternary search.

> tsearch :: (Int -> Int) -> Int -> Int -> Int
> tsearch f lo hi
>   | hi - lo < 3 = minimum $ f <$> [lo..hi-1]
>   | f a < f b = tsearch f lo b
>   | otherwise = tsearch f a hi
>   where a = (2*lo + hi) `div` 3
>         b = (lo + 2*hi) `div` 3

In part 1, the cost is exactly the distance.

> part1 :: [Int] -> Int
> part1 ps = tsearch (sumDistTo ps id) (minimum ps) (maximum ps + 1)
>
> sumDistTo :: [Int] -> (Int -> Int) -> Int -> Int
> sumDistTo ps f p = sum [ f (abs (p'-p)) | p' <- ps ]

In part 2, the cost is a staircase function of “sum of the step
indices”.

> staircase :: Int -> Int
> staircase = (listArray (0,1999) cs !) where cs = 0 : zipWith (+) cs [1..]
>
> part2 :: [Int] -> Int
> part2 ps = tsearch (sumDistTo ps staircase) (minimum ps) (maximum ps + 1)

A short wrapper to perform it all.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . map read . wordsBy (== ',')

This concludes the coded aspect of today's solution.  See you soon!
