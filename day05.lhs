---
title: "AoC Day 5: Hydrothermal Venture"
author: Jean-Baptiste Mazon
date: 2021-12-05T14:58:56+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Just implement it.
image: aoc-haskell.jpeg
---

For [day 5 of Advent of Code][aoc], we are to count distinct
intersections between lines.  In the general case, this would be a
tricky problem whose naïve solution is quadratic.  But here, we're
lucky enough to have the following simplifiers:

[aoc]: https://adventofcode.com/2021/day/5

* we're only handling grid-aligned lines (horizontal, vertical, diagonal)
* the grid is small enough

So we can just draw the lines and count, and have a solution linear in
grid area instead of quadratic in numbre of lines.  Which, given my
puzzle input, is more or less the same anyway.

This post is literate Haskell; let's get the imports out of the way.

> import Control.Arrow   ((&&&),(***))
> import Data.Array      (accumArray,elems)
> import Data.List.Split (wordsBy)

As far as I could tell, the coordinates in my input are all between 0
and 1000, so they fit in a standard `Int`.

> type Coords = (Int,Int)
> 
> readCoords :: String -> Coords
> readCoords (wordsBy (== ',') -> [x,y]) = (read x,read y)
> 
> type Line = (Coords,Coords)
> 
> parse :: String -> Line
> parse (words -> [a,"->",b]) = (readCoords a,readCoords b)

I'm abusing ViewPatterns [again][vp], though this time it seems
slightly more reasonable.

[vp]: /posts/2021-12-aoc/day02.html

Next we need to be able to convert a line to the list of integer
coordinates it lies on.

> line2coords :: Line -> [Coords]
> line2coords ((x1,y1),(x2,y2)) = [ (x1+i*dx,y1+i*dy) | i <- [0 .. max w h ] ]
>   where (dx,w) = sgnAbs (x2-x1)
>         (dy,h) = sgnAbs (y2-y1)
>         sgnAbs = signum &&& abs

We now have all we need to “draw” the lines on a virtual grid,
counting the number of passages.

> count :: [Line] -> Int
> count = length . filter (>= 2) . elems .
>         accumArray (+) 0 ((0,0),(999,999)) .
>         flip zip (repeat (1 :: Int)) .
>         concatMap line2coords

For part 1, we only consider horizontal and vertical lines, so I need
a discriminator for that.

> isHV :: Line -> Bool
> isHV ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

And a simple wrapper to do it all.

> main :: IO ()
> main = interact $ show . (curry (count *** count) =<< filter isHV) .
>                   map parse . lines

This concludes today's solution.  See you tomorrow!
