---
title: "AoC Day 25: Sea Cucumber"
author: Jean-Baptiste Mazon
date: 2021-12-25T20:07:47+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Just code it
image: aoc-haskell.jpeg
---

Advent of Code 2021's final puzzle, [“Sea Cucumber”][aoc], is a
straightforward implementation problem that doesn't warrant much more
explanation than just coding it.  We're sticking to [literate
Haskell][gh] for this ultimate mainlist arc post.

[aoc]: https://adventofcode.com/2021/day/25
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day25.lhs

Sea cucumbers are provided on a rectangular^[Or is it square?] grid
that has torus-like topological properties.  “I know, I'll use an
array!”

> import Data.Array
> type Pos = (Int,Int)

Sea cucumbers come in two herds: those that move eastward and those
that move southward.

> data Dir = East | South deriving Eq

The puzzle input contains the herds' initial spread.

> parse :: String -> Array Pos (Maybe Dir)
> parse (lines -> ls) = listArray ((0,0),(h-1,w-1)) (map p (concat ls))
>   where p '>' = Just East
>         p 'v' = Just South
>         p '.' = Nothing
>         w = length (head ls)
>         h = length ls

The east-bound herd moves first.  Cucumbers only move if the space
ahead of them is clear: there is no group tailing.

> stepRight :: Int -> Array Pos (Maybe Dir) -> Int
> stepRight i g = stepDown i (null changes) (g // changes)
>   where changes = moves g East

After performing all simultaneous eastward moves possible, we check
and remember whether anything moved at all.

Next come the southward moves.  They behave the same, really, barring
the axis they check spaces on.

> stepDown :: Int -> Bool -> Array Pos (Maybe Dir) -> Int
> stepDown i warned g
>   | warned && null changes = i
>   | otherwise = stepRight (i+1) (g // changes)
>   where changes = moves g South

We also make note of whether any move happened; if neither east- nor
southward moved, we're done and return the turn number.

A little helper to generate the simultaneous moves list:

> moves :: Array Pos (Maybe Dir) -> Dir -> [(Pos,Maybe Dir)]
> moves g dir = concat
>     [ [(dst,Just dir),(src,Nothing)]
>     | (dst,Nothing) <- assocs g
>     , let src = from g dir dst
>     , g!src == Just dir
>     ]

And an auxiliary function to determine where a free space could get
moved on from:

> from :: Array Pos (Maybe Dir) -> Dir -> Pos -> Pos
> from g = \case East  -> \(i,j) -> (i,(j-1) `mod` w)
>                South -> \(i,j) -> ((i-1) `mod` h,j)
>   where (succ -> h,succ -> w) = snd (bounds g)

A trivial wrapper to seed the initial turn count, and we're done!

> main :: IO ()
> main = interact $ show . stepRight 1 . parse

This concludes this year's solutions.  I'll allow myself a week's
break and come back for the retrospective.  See you then!
