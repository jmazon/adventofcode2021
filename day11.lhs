---
title: "AoC Day 11: Dumbo Octopus"
author: Jean-Baptiste Mazon
date: 2021-12-11T09:41:54+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: The dance of the writers
image: aoc-haskell.jpeg
---

Day 11, [“Dumbo Octopus”][aoc], is the closest we're getting to a
cellular automaton in the present state of this year's Advent of Code.
I'll be using various incarnations of the Writer monad, imported here.

[aoc]: https://adventofcode.com/2021/day/11
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day11.lhs

> import           Control.Applicative (liftA2)
> import           Control.Arrow ((&&&),(***))
> import           Control.Monad (guard,(>=>))
> import           Control.Monad.Writer.Class (tell)
> import qualified Control.Monad.Writer.Lazy as L
> import qualified Control.Monad.Writer.Strict as S
> import           Data.Array
> import           Data.Char (digitToInt)
> import           Data.List (elemIndex)
> import           Data.Maybe (fromMaybe)
> import           Data.Monoid (Sum(Sum))

The octopuses are conveniently arranged in a 2D grid.

> type OctoGrid = Array (Int,Int)

To handle the flashing, I'll enhance the octopuses' energy level to
remember whether they flashed in this step: I wouldn't want to let
them flash twice, messing up with my counting.

> flash :: OctoGrid (Maybe Int) ->

I'll call this flash function iteratively until there's no octopus
left with a high enough energy level.  So I'll aggregate the tally of
flashes using a (strict) writer monad over a `Sum` monoid.

>         S.Writer (Sum Int) (OctoGrid (Maybe Int))

The octopuses that should flash are those with an energy level over
$9$.  The octopuses that just flashed are now valued at `Nothing`,
which compares under `Just 9`.

> flash g = case filter ((> Just 9) . (g!)) (indices g) of

The easy case is the termination one: just return the grid as is, and
don't iterate any more.

>             [] -> pure g

If there are octopuses scheduled to flash, count them, update the
grid, and iterate.

>             flashing -> tell (Sum (length flashing)) *>
>                         flash (accum (liftA2 (+)) g (concatMap dazzle flashing))

Updating the grid adds one to the neighbors' energy level, and marks
the flasher away.

>   where dazzle (i,j) = [ ((i',j'),1 <$ guard ((i',j') /= (i,j)))
>                        | i' <- [i-1..i+1], j' <- [j-1..j+1]
>                        , inRange (bounds g) (i',j') ]

With this simple building block, I can implement the full step by
function composition.  In order:

* expand the energy level type and increase it
* iteratively flash as long as it has to
* aggregate the flash count
* reduce to a simple energy level
* report the step's flash count

> step :: OctoGrid Int -> L.Writer [Sum Int] (OctoGrid Int)
> step = L.writer . (fmap (fromMaybe 0) *** pure) .
>        S.runWriter . flash . fmap (Just . succ)

Iterating steps yields a full simulation, returning the flash count
per step.  As it's an infinite list, I use a lazy writer there.

> simulate :: OctoGrid Int -> [Sum Int]
> simulate = L.execWriter . go where go = step >=> go

Part 1 asks for the total flash count in 100 turns.

> part1 :: [Sum Int] -> Sum Int
> part1 = mconcat . take 100

Part 2 asks for the first step number that flashes them all.  (`succ`
to convert from 0-based list indexing to human ordinals)

> part2 :: [Sum Int] -> Maybe Int
> part2 = fmap succ . elemIndex (Sum 100)

Here's the wrapping code for completeness.

> parse :: String -> OctoGrid Int
> parse = listArray ((1,1),(10,10)) . map digitToInt . concat . lines
> 
> main :: IO ()
> main = interact $ show . (part1 &&& part2) . simulate . parse

[The literate Haskell][gh] program is on GitHub as usual.  See you
tomorrow!
