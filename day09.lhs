---
title: "AoC Day 9: Smoke Basin"
author: Jean-Baptiste Mazon
date: 2021-12-09T07:48:31+01:00
updated: 2021-12-09T15:36:29+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: An induced DFS
image: aoc-haskell.jpeg
---

Advent of Code day 9 [“Smoke Basin”][aoc] has us study the flow of
smoke on uneven terrain.  As usual, this post is a [literate Haskell
program][gh] that starts with a few imports.

[aoc]: https://adventofcode.com/2021/day/9
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day09.lhs

> import Control.Arrow ((&&&))
> import Data.Array
> import Data.Char (digitToInt)
> import Data.List (group,minimumBy,sort,sortOn)
> import Data.Ord (comparing,Down(Down))


I'll represent the Cave as an 2D array, so defining a few types will
spare typing.

> type Pos = (Int,Int)
> type Height = Int
> type Cave = Array Pos

Finding a low point is a matter of examining each point and validating
its context for minimality.^[Yes, I'm aware this is an invitation to
comonads.]

> lowPoints :: Cave Height -> [(Pos,Height)]
> lowPoints c = filter f (assocs c) where
>   f ((i,j),h) = h < minimum (snd <$> neighbors c (i,j))

We can then easily compute the low points' risk and sum to complete
part 1.

> risk :: (Pos,Height) -> Int
> risk (_,h) = h + 1

For part 2, we want to compute the actual basins' areas.  They're
conveniently defined as having a single low point, excluding
height 9.[^why] So I'll compute the final low point for every point of
a basin, defining it recursively as either itself when it's a low
point, or as the low point of its lowest neighbor.[^how]

[^why]: Because it avoids having to specify what happens if
multiple flow directions are possible.  I read right through it, yet
appreciate it's masterfully done.

[^how]: Picking the lowest explores all neighbor nodes, but reduces
the mean distance to the final low point.  But only by a constant
factor.  As descent bounds that distance to 9 anyway, trying to
compare big-Os is a true bigger waste of time than anything that could
be lost here.

> destinationLP :: Cave Height -> Cave Pos
> destinationLP c = d where
>   d = array (bounds c) [ (p,dest p) | p <- indices c ]
>   dest p | c!p == 9 = p
>          | c!p < snd l = p
>          | otherwise = d ! fst l
>     where ns = neighbors c p
>           l = minimumBy (comparing snd) ns

I cheat and make height 9 points their own microbasin, so that they
don't get in the way when I aggregate by low point:

> basins :: Cave Height -> [[Pos]]
> basins = group . sort . elems . destinationLP

The rest is just wrapping and utility functions.

> main :: IO ()
> main = interact $ show .
>                 ( sum . map risk . lowPoints &&&
>                   product . take 3 . sortOn Down . map length . basins ) .
>                 parse
>
> neighbors :: Cave Height -> Pos -> [(Pos,Int)]
> neighbors c (i,j) = [ (p,c!p) | p <- [(i-1,j),(i,j+1),(i+1,j),(i,j-1)]
>                               , inRange (bounds c) p ] 
>
> parse :: String -> Cave Height
> parse = listArray ((0,0),(99,99)) . (concatMap.map) digitToInt . lines

This concludes today's solution.  See you tomorrow!
