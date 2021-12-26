---
title: "AoC Day 19: Beacon Scanner"
author: Jean-Baptiste Mazon
date: 2021-12-19T15:14:10+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Twist and turn and beacon align
image: aoc-haskell.jpeg
---

In [“Beacon Scanner”][aoc], the Advent of Code day 19 puzzle, we are
to make sense of a series of scanner logs.  This post is [literate
Haskell][gh] with a “few” imports to get the ball rolling.

[aoc]: https://adventofcode.com/2021/day/19
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day19.lhs

> import Control.Applicative       (liftA2)
> import Control.Arrow             ((***))
> import Control.Lens
> import Data.Maybe                (mapMaybe,listToMaybe)
> import Control.Monad             (guard,join)
> import Data.List                 (delete)
> import Data.List.Split           (splitOn,wordsBy)
> import qualified Data.IntMap as IMap
> import qualified Data.IntSet as ISet
> import qualified Data.Set as Set

The logs are given as a list of blips, as seen relative to the
scanner's position and orientation.  The scanners have a [Chebyshev
range][che] of 1 000 that's mostly irrelevant to the solution process.
Still, we'll be shifting coordinate systems around a lot, so let's
cover our bases.

[che]: https://en.wikipedia.org/wiki/Chebyshev_distance

> import Linear.Matrix
> import Linear.V3
> import Linear.Vector
>
> manhattan :: V3 Int -> V3 Int -> Int
> manhattan = ((sum . abs) .) . subtract

Scanner logs are provided in an ad-hoc format for which the following
parser works—trust me on this; it's really not the most interesting
part of the puzzle.

> data Scanner = Scanner
>   { scannerId :: !Int
>   , scannerBlips :: [V3 Int]
>   }
>
> parse :: String -> [Scanner]
> parse = map (parseScanner . lines) . splitOn "\n\n" where
>   parseScanner (h:t) = Scanner (parseHeader h) vs
>     where vs = map (toV3 . parseV) t
>   parseHeader (words -> ["---","scanner",n,"---"]) = read n
>   toV3 es = zero & partsOf traversed .~ es
>   parseV = map read . wordsBy (== ',')

Judging by the Twitter Zeitgeist at time of solving, generating the
set of 24 possible orientations was not an obvious endeavour.  My
approach is to pick a first axis (`dir1`) from a canonical 3-basis, a
distinct second anonymous axis, generate two directions
(`[id,negated]` yielding `v1` and `v2`) for each and deduce the third
basis vector by cross product.

> orientations :: [M33 Int]
> orientations = do
>   dir1 <- basis
>   v1 <- [id,negated] ?? dir1
>   v2 <- [id,negated] <*> delete dir1 basis
>   pure (transpose (V3 v1 v2 (cross v1 v2)))

It's not often I get to wield `(??)`; please hold on for a minute
while I enjoy the moment.

---

Thank you for your understanding.

Now comes the core operation: given two scanner logs, can we decide
whether they overlap, and by how much?  The simple answer comes from
the problem statement: we want to try to match them over every
possible pair of orientations the scanners may have.  But that's a bit
much: we can reduce it to every possible (single) relative
orientation.  For each of those, we need to find candidate offsets,
and see whether or not subtracting it to one side of the blips brings
twelve of them in the same position as blips from the other side.  How
do we get likely candidate offsets?  By trying every pair of points,
one per scanner.

It's tractable: that's 24 orientations times around 26 blips on one
radar times say 27 blips on the other for the orientation-offset
candidate, then up to 26 blips to transform and check for presence on
the other side.  On top of the already quadratic-by-default pairwise
scanner matching algorithm.

There's a huge speedup to gain by optimizing the common case of a
scanner mismatch: what property can we find to quickly be in a
position to say: “these two scanners can't match”?  A simple one would
be a internal measure of shape.  For example we can measure all
distances between blips seen by each scanner.  There are twelve to
find in common: those would translate in 66 (no self, no duplicates)
to 144 (selves and duplicates, easier code) distances to find in
common between two scanners.

> fastFilter :: MonadFail m => Scanner -> Scanner -> m ()
> fastFilter s1 s2
>   | bagIntersect ds1 ds2 >= (144 :: Int) = pure ()
>   | otherwise = fail "Not enough internal similarity"
>   where ds1 = innerDistances s1
>         ds2 = innerDistances s2
>         bagIntersect = (sum .) . IMap.intersectionWith min
>         innerDistances = IMap.fromListWith (+) . flip zip (repeat 1) .
>                          join (liftA2 manhattan) . scannerBlips

I'll extend it to a fast (on average) reliable (always) scanner
overlap checker.  In case of a match, I return the offset and
orientation matrix—the second scanner's basis expressed in the first
one's coordinates.

> checkOverlap :: Scanner -> Scanner -> Maybe (M33 Int,V3 Int)
> checkOverlap s1 s2 = listToMaybe $ do
>   fastFilter s1 s2
>   m <- orientations
>   let s1' = scannerBlips s1
>       s2' = (m !*) <$> scannerBlips s2
>   v1 <- s1'
>   v2 <- s2'
>   let v = v1 - v2
>       s2'' = (+ v) <$> s2'
>   guard (length (filter (`notElem` s1') s2'') >= 12)
>   pure (m,v)

I can now go through all scanners and try to connect them to those I
already grouped together.  As the resulting graph is necessarily
connected, a simple [DFS][dfs] suffices.

[dfs]: https://en.wikipedia.org/wiki/Depth-first_search

> connectAll :: [Scanner] -> (Set.Set (V3 Int),[V3 Int])
> connectAll scanners =
>     dfs ISet.empty [(identity,zero,head scanners)]
>   where
>     dfs _ [] = mempty
>     dfs cl ((m,v,s):q)
>       | scannerId s `ISet.member` cl = dfs cl q
>       | otherwise = localInfo <> dfs cl' (q' ++ q)
>       where cl' = ISet.insert (scannerId s) cl
>             q' = mapMaybe (toNext =<< checkOverlap s) $
>                  filter ((`ISet.notMember` cl') . scannerId) scanners
>             localInfo = (Set.fromList ((\b -> m !* b + v) <$> scannerBlips s),[v])
>             toNext mb s' = (\(m',v') -> (m !*! m', m !* v' + v,s')) <$> mb

The aggregated information per node is:

1. the set of blips, normalized to first scanner's viewpoint
2. the scanner's origin

The set of blips is useful in counting the total number of visible
beacons:

> part1 :: Set.Set a -> Int
> part1 = Set.size

The scanners' origins are useful in finding out the biggest pairwise
distance:

> part2 :: [V3 Int] -> Int
> part2 = maximum . (liftA2 manhattan =<< id)

A simple wrapper binds it all.

> main :: IO ()
> main = interact $ show . (part1 *** part2) . connectAll . parse

This concludes today's solution.  See you tomorrow!
