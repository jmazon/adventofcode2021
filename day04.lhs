---
title: "AoC Day 4: Giant Squid"
author: Jean-Baptiste Mazon
date: 2021-12-04T18:36:48+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: And squiddo was his name-o.
image: aoc-haskell.jpeg
---

[Today's Advent of Code][aoc] asks us to solve bingo.  Let's start
with a few import to serve as a [literate Haskell][gh] prologue.

[aoc]: https://adventofcode.com/2021/day/4
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day04.lhs

> import Control.Arrow   ((&&&))
> import Control.Monad   (guard)
> import Data.List       (partition,transpose)
> import Data.List.Split (chunksOf,wordsBy)
> import Data.Maybe      (catMaybes)

We'll be marking out numbers from bingo boards, so I'll represent them
as lists of lists of `Maybe Int`s, where `Nothing` would represent a
marked number.

> type Board = [[Maybe Int]]

The input format is very readable, which is all too often synonymous
with painful to parse.  For simplicity, I'll assume the caller would
already have it split by words, which separates the numbers' order
nicely as a huge first word.  I then split it on commas, and form the
boards by repeated application of `chunksOf 5`.

> parse :: [String] -> ([Int],[Board])
> parse ( order : boards ) =
>   ( read <$> wordsBy (== ',') order
>   , chunksOf 5 (chunksOf 5 (Just . read <$> boards)) )

Marking can be implemented as a simple comparison. A guard-like
function is a nice fit, though not really the core of the matter here.

> mark :: Int -> Board -> Board
> mark n board = (map . map) (guarded' (/= n)) board
>   where guarded' p mx = do { x <- mx ; x <$ guard (p x) }

Checking a board for a win is now a simple matter of looking for a row
or column entirely made up of `Nothing`s.

> isWinning :: Board -> Bool
> isWinning g = replicate 5 Nothing `elem` (g ++ transpose g)

Scoring a winning board can then be implemented by flattening the
board, summing and multiplying.

> score :: Int -> Board -> Int
> score n g = n * (sum . catMaybes . concat) g

Part 1 asks for the first board to win, part 2 for the last one, so
let's just compute all of their scores in order.

> winners :: [Int] -> [Board] -> [Int]
> winners _ [] = []
> winners (n:ns) bs = map (score n) won ++ winners ns bs'
>   where (won,bs') = partition isWinning (mark n <$> bs)

And combine it all in a simple wrapper.

> main :: IO ()
> main = interact $ show . (head &&& last) . uncurry winners . parse . words

This concludes today's solution.  See you tomorrow!
