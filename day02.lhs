---
title: "AoC Day 2: Dive!"
author: Jean-Baptiste Mazon
date: 2021-12-02T12:00:00+01:00
updated: 2021-12-05T00:40:13+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Making a fool of myself with lenses once again
image: aoc-haskell.jpeg
---

[The day 2 puzzle][aoc] is another of these “follow an instruction
sequence using two different interpretations”.  This post is literate
Haskell, so let's get the imports out of the way.

[aoc]: https://adventofcode.com/2021/day/2

> {-# LANGUAGE TemplateHaskell #-}
> import Control.Arrow        ((&&&),(***))
> import Control.Lens         (makeLenses,Getter,(&),(+~),to,view)
> import Control.Lens.Unsound (lensProduct)
> import Data.Semigroup       (Dual(..),Endo(..))

The code defines three instructions, but two of them (`up` and `down`)
behave symmetrically, so I'll merge their internal representation.

> data Command = Horiz Int | Vert Int
>
> parse :: String -> Command
> parse (words -> [cmd,read -> i]) = case cmd of
>   "forward" -> Horiz  i
>   "down"    -> Vert   i
>   "up"      -> Vert (-i)

Every time I try to use ViewPatterns, I'm disappointed with the
post-hoc readability.  Oh well.

In part 1, the commands' interpretation is straightforward, summing
bidimensional moves.

> data State1 = S1 { _horiz :: !Int, _depth :: !Int }
> makeLenses ''State1
>
> pos1 :: State1
> pos1 = S1 0 0
>
> part1 :: Command -> Endo State1
> part1 = Endo . \case Horiz i -> horiz +~ i
>                      Vert i  -> depth +~ i
>
> display :: Getter State1 Int
> display = lensProduct horiz depth . to (uncurry (*))

Feels weird to reach for Lens.Unsound for a simple getter product.
That's what I get for using lenses once per year.  Improvements
welcome.

In part 2, the interpretation gets weird.  We still have a position,
but are invited to keep track of an additional value: the *aim*.

> data State2 = S2 { _pos :: !State1, _aim :: !Int }
> makeLenses ''State2
>
> pos2 :: State2
> pos2 = S2 pos1 0

It's still implemented as a simple case match.  You'll note I use the
`Dual` monoid adaptor: `Endo` composes in the usual mathematical
direction, resulting in functions being applied right to left.  It
didn't matter for part 1 where they commuted, but here we really have
to apply them left to right.

> part2 :: Command -> Dual (Endo State2)
> part2 = Dual . Endo . \case Vert i  -> aim +~ i
>                             Horiz i -> depthChange i . (pos.horiz +~ i)
>   where depthChange i st = st & pos.depth +~ view aim st * i

That `depthChange` function is also a bit disappointing: I was hoping
to write it without making `st` explicit.  Improvements welcome.

Anyway, a wrapper to run the whole of it and we're done.

> main :: IO ()
> main = interact $ show .
>                   (view display     *** view (pos.display)) .
>                   ((`appEndo` pos1) *** (`appEndo` pos2) . getDual) .
>                   foldMap (part1 &&& part2) .
>                   map parse . lines

This concludes today's solution.  See you tomorrow!
