---
title: "AoC Day 22: Reactor Reboot"
author: Jean-Baptiste Mazon
date: 2021-12-22T16:21:02+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: CSG with irregular voxels
image: aoc-haskell.jpeg
---

For day 22 of Advent of Code, the [“Reactor Reboot”][aoc] problem asks
us to count the number of active voxels after a simplified series of
[CSG][csg] operations.  This post is [literate Haskell][gh] and these
are its imports.

[aoc]: https://adventofcode.com/2021/day/22
[csg]: https://en.wikipedia.org/wiki/Constructive_solid_geometry
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day22.lhs

> import Control.Arrow    ((&&&))
> import Data.Array.Unboxed hiding ((!))
> import Data.Foldable    (foldMap')
> import Data.List        (group,sort)
> import Data.Monoid      (Sum(Sum))
> import Data.Vector      ((!))
> import Text.Regex.Posix ((=~),getAllTextSubmatches)
> import qualified Data.Vector as V

The problem space is cartesian-indexed 3D voxels.

> type Pos = (Int,Int,Int)

The operations are provided as a series of activations or
deactivations on a specific rectangular cuboid.

> type Step = (Bool,(Pos,Pos))

The initialization sequence runs on a space of volume $100^3$, that
easily fits in RAM, packed or not.  So let's just run it directly
there, on an `Array` of `Bool`s.

> apply :: [Step] -> Array Pos Bool
> apply = accumArray (flip const) False ((-50,-50,-50),(50,50,50)) .
>         concatMap (\(st,r) -> zip (range r) (repeat st))

The hardest part of the problem may be filtering the actual steps that
matter for the initialization phase.  I do appreciate Eric not playing
with edge cases of any kind there: all steps provided in the input are
either entirely within the init section or entirely outside.

> filterInit :: Step -> Bool
> filterInit (_,((x1,y1,z1),(x2,y2,z2))) =
>   minimum [x1,y1,z1] >= -50
>   && maximum [x2,y2,z2] <= 50

Putting it all together, part 1's a breeze.

> part1 :: [Step] -> Int
> part1 = length . filter id . elems . apply . filter filterInit

For part 2, things get trickier: there are no given bounds on the
observed region, and in input they're in the order of
$200\,000^3=8\times 10^{15}$, which definitely doesn't fit in (my)
RAM.

But the input itself is not as large.  Mine is only 420 lines
long. This means there are at most 840 possible divisions along each
axis.  So if we were to count pseudovoxels only along those planes,
we'd have about $840^3\approx 6\times 10^8$ of them to manage.  Which
is large, but not intractable.

Let's redefine the data structures along this new coordinate space.

> newtype Int' = I' Int deriving (Eq,Ord,Ix,Show)
> type Pos' = (Int',Int',Int')
> type Step' = (Bool,(Pos',Pos'))

Now to convert to and from, we'll need the actual division points.

> type Split = (V.Vector Int,V.Vector Int,V.Vector Int)
> 
> splitPoints :: [Step] -> Split
> splitPoints ss = (V.fromList (nub' xs),V.fromList (nub' ys),V.fromList (nub' zs))
>   where
>     ps = concatMap (\(_,(p1,(x2,y2,z2))) -> [p1,(x2+1,y2+1,z2+1)]) ss
>     (xs,ys,zs) = unzip3 ps
>     nub' = map head . group . sort

The tricky aspect is the off-by-ones: cuboids are provided with
inclusive bounds, so:

* a cuboid's lower bound means “split before”
* a cuboid's upper bound means “split after”

So the split table maps indices to “split befores”, and the rest of
the code adjusts around the “split afters”:

* generating the split table converts an upper bound to a split before
  a virtual lower bound of +1 on every axis
* converting a step to compact space looks up the same +1

> convertSplit :: Split -> Step -> Step'
> convertSplit (xs,ys,zs) (st,((x1,y1,z1),(x2,y2,z2)))
>   = (st,((x1',y1',z1'),(x2',y2',z2')))
>   where Just x1' = I' <$> V.elemIndex x1 xs
>         Just y1' = I' <$> V.elemIndex y1 ys
>         Just z1' = I' <$> V.elemIndex z1 zs
>         Just x2' = I' . subtract 1 <$> V.elemIndex (x2+1) xs
>         Just y2' = I' . subtract 1 <$> V.elemIndex (y2+1) ys
>         Just z2' = I' . subtract 1 <$> V.elemIndex (z2+1) zs

With the split vectors at hand, conversion was basically a lookup.  

Applying the operations now works the same, just on the dual type.

> apply' :: Split -> [Step'] -> UArray Pos' Bool
> apply' (xs,ys,zs) =
>     accumArray (flip const) False
>       ( (I' 0,I' 0,I' 0)
>       , (I' (length xs - 1),I' (length ys - 1),I' (length zs - 1)) ) .
>     concatMap (\(st,r) -> zip (range r) (repeat st))

Counting the volumes to yield the result is obviously a bit more
complex: we're not counting voxels anymore, but summing their expanded
volumes instead.

> count :: Split -> UArray Pos' Bool -> Sum Int
> count (xs,ys,zs) a = foldMap' volume (fst <$> filter snd (assocs a))
>   where
>     volume (I' x',I' y',I' z') = Sum (δx * δy * δz)
>      where δx = xs!(x'+1) - xs!x'
>            δy = ys!(y'+1) - ys!y'
>            δz = zs!(z'+1) - zs!z'

The part 2 wrapper is similar to the previous one.  Most notably the
split vectors are passed along to most operations.^[Please don't
suggest I “simplify” things with a Reader monad.]

> part2 :: [Step] -> Sum Int
> part2 steps = count s $ apply' s $ convertSplit s <$> steps
>   where s = splitPoints steps

The rest of the code is presented for completeness, but not so
interesting.

> parse :: String -> [Step]
> parse = map parseStep . lines where
>   parseStep l = (st,((x1,y1,z1),(x2,y2,z2))) where
>     (_:st_:rawCoords) = getAllTextSubmatches $
>       l =~ ( "(on|off) x=" ++ n ++ ".." ++ n ++
>                      ",y=" ++ n ++ ".." ++ n ++
>                      ",z=" ++ n ++ ".." ++ n )
>     st = st_ == "on"
>     [x1,x2,y1,y2,z1,z2] = read <$> rawCoords
>   n = "(-?[0-9]+)"
> 
> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

The code runs in about 50 seconds, which I think is by far the longest
so far.  But it was feasible with very simple code, skirting the CSG
issue altogether, which I think has a lot of intrinsic value.  Yet
another engineering vs runtime tradeoff, I guess.

But I may revisit! 😀

This concludes today's solution.  See you tomorrow!
