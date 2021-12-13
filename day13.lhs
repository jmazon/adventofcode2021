---
title: "AoC Day 13: Transparent Origami"
author: Jean-Baptiste Mazon
date: 2021-12-13T11:36:38+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Folding folds
image: aoc-haskell.jpeg
---

Day 13, [“Transparent Origami”][aoc], is the day of the human OCR.
This post is your usual [literate Haskell][gh], with a few imports to
start with.

[aoc]: https://adventofcode.com/2021/day/13
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day13.lhs

> import Control.Arrow ((&&&),(***),second)
> import Data.List     (nub)
> import qualified Data.Set as Set

The puzzle input is a list of points and a list of folds.  Let's define
simple types for those.

> type Point = (Int,Int)
> type Fold = Either Int Int -- ^ x fold ⊕ y fold

A fold behaves just like you'd expect from a piece of paper.  Here's
one way to implement it:

> applyFold :: Point -> Fold -> Point
> applyFold (x,y) (Left  x0) = (x0 - abs(x0-x),y)
> applyFold (x,y) (Right y0) = (x,y0 - abs(y0-y))

In part 1, we're only asked to fold once, then count the remaining
number of distinct visible points after the colocated ones have
merged.

> part1 :: [Point] -> Fold -> String
> part1 points fold = show $ length $ nub $ (`applyFold` fold) <$> points

In part 2, we perform the full fold sequence and try to recognize the
code.  I'll only fold and present in code, the character recognition
being left as an exercise to the user.

> part2 :: [Point] -> [Fold] -> [String]
> part2 points folds = render $ flip (foldl applyFold) folds <$> points

The rendering does warrant a word or two.  I want to keep it agnostic
to paper size.  So I'll first scan through the resulting folded
points, aggregating:

1. full point set
2. topmost or leftmost encountered coordinate
3. bottommost or rightmost encountered coordinate

> render :: [Point] -> [String]
> render = raster . scan where
>   scan = foldMap (Set.singleton &&& Just . Min2D &&& Just . Max2D)

Then I'll sweep the resulting matrix to generate lines:

>   raster (s,(Just (Min2D (xl,yl)),Just (Max2D (xh,yh)))) =
>     [ [ if (x,y) `Set.member` s then '#' else ' '
>       | x <- [xl..xh]
>       ]
>     | y <- [yl..yh]
>     ]

I'm using two straightforward `Semigroup` instances to manage two axes
at once.

> newtype Min2D = Min2D Point
> instance Semigroup Min2D where
>   Min2D (a,b) <> Min2D (c,d) = Min2D (min a c,min b d)
> newtype Max2D = Max2D Point
> instance Semigroup Max2D where
>   Max2D (a,b) <> Max2D (c,d) = Max2D (max a c,max b d)

The rest is parsing and wrapping, that doesn't really need any more
explanation at this point in the month.

> main :: IO ()
> main = interact $ unlines . uncurry (:) .
>                   ( uncurry part1 . second head &&&
>                     uncurry part2
>                   ) . parse
> 
> parse :: String -> ([Point],[Fold])
> parse = (map readPoint *** map readFold . tail) . break null . lines
> 
> readPoint :: String -> Point
> readPoint (break (== ',') -> (x,',':y)) = (read x,read y)
> 
> readFold :: String -> Fold
> readFold (words -> ["fold","along",f]) = case f of
>   'x':'=':n -> Left  (read n)
>   'y':'=':n -> Right (read n)

This concludes today's solution.  See you tomorrow!
