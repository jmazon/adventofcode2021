---
title: "AoC Day 15: Chiton"
author: Jean-Baptiste Mazon
date: 2021-12-15T11:28:57+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Dijkstra's famous risk-free path algorithm
image: aoc-haskell.jpeg
---

I'm tainted.  Since Code Jam's problem of the same name, I can't
dissociate Dijkstra from hamiltonians anymore.  Anyway, for day 15 of
Advent of Code, [“Chiton”][aoc], we are to find the less risky path
from a corner of the cave to the other.  Which can definitely be
interpreted as a form of shortest path, so that's exactly what we'll
do.  This post is [literate Haskell][gh], and these are its imports.

[aoc]: https://adventofcode.com/2021/day/15
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day15.lhs

> import Data.Array    (Array,array,listArray,(!),bounds,range,inRange)
> import Control.Arrow ((&&&))
> import Data.Char     (digitToInt)
> import Data.Set      (empty,singleton,insert,deleteFindMin,member,notMember)

The cave is 2D; risk is a single-digit integer, there is one per
position in the cave.

> type Pos = (Int,Int)
> type Risk = Int
> type Cave = Array Pos Risk

The input is supposed to be square, I'm not really verifying it.
Moreover, I'm overengineering and separating width from height.

> parse :: String -> Array Pos Risk
> parse (lines -> ls@(l1:_)) =
>   let w = length l1
>       h = length ls
>   in (listArray ((0,0),(h-1,w-1)) . map digitToInt . concat) ls

Dijkstra's algorithm is a specialization of the generic graph
traversal algorithm where the nodes are expanded in order of distance
to the origin.  It's guaranteed to find a path if one exists; it's
guaranteed to be the shortest if the edge weights are all $\ge 0$.

I'm implementing it with a simple ordered `Set` because for the kind
of dimensions in the puzzle that won't make much of a difference.

> dijkstra :: Cave -> Risk
> dijkstra g = go empty (singleton (0,(0,0))) where
>   go cl (deleteFindMin -> ((d,p),q))
>     | p == snd (bounds g) = d
>     | p `member` cl       = go cl  q
>     | otherwise           = go cl' q'
>     where
>       cl' = insert p cl
>       q' = foldl (flip insert) q
>         [ (d + g!p',p') | p' <- neighbors p, p' `notMember` cl ]
>   neighbors (i,j) = [ p | p <- [ (i-1,j),(i,j+1),(i+1,j),(i,j-1) ]
>                       , inRange (bounds g) p ]

Part 2 is the same problem on a somewhat larger cave.  The gist of the
code to add is to generate it from the small one.

> tile :: Cave -> Cave
> tile g = array bds
>   [ (p,1 + (g!(i',j') + ii + jj - 1) `mod` 9)
>   | p@(i,j) <- range bds
>   , let (ii,i') = i `divMod` h
>   , let (jj,j') = j `divMod` w
>   ]
>   where bds = ((u,l),(u+5*w-1,l+5*w-1))
>         ((u,l),(b,r)) = bounds g
>         h = b - u + 1
>         w = r - l + 1

A little wrapping, and we're done.

> main :: IO ()
> main = interact $ show . (dijkstra &&& dijkstra . tile) . parse

That was a quick one.

This concludes today's solution.  See you tomorrow!
