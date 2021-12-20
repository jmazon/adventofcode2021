---
title: "AoC Day 17: Trick Shot"
author: Jean-Baptiste Mazon
date: 2021-12-17T14:38:51+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Balancing compute and engineering time
image: aoc-haskell.jpeg
flags: [ mathjax ]
---

<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" type="text/javascript" async></script>

Day 17 of Advent of Code, [“Trick Shot”][aoc], asks us for information
about shooting an object at a target under specific physics.  It is
peculiar in that I didn't reach for code until part 2.  So what you'll
read here, as a part of a Haskell series, is a retro-implementation of
it.  Still as [literate Haskell][gh], with a few imports that
decidedly have nowhere else to go than the start:

[aoc]: https://adventofcode.com/2021/day/17
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day17.lhs

> import Control.Arrow    ((&&&))
> import Data.Ix          (range,inRange)
> import Text.Regex.Posix ((=~),AllTextSubmatches(..))

Let's have a bit of regex parsing, for a change.

> data Bounds = Bounds
>   { xl :: !Int
>   , xr :: !Int
>   , yb :: !Int
>   , yt :: !Int
>   }
>
> parse :: String -> Bounds
> parse l = Bounds{..} where
>   [_,xl,xr,yb,yt] = map read . getAllTextSubmatches $
>     l =~ "target area: x=([0-9]+)\\.\\.([0-9]+), y=(-[0-9]+)\\.\\.(-[0-9]+)"

For part 1, we are asked the highest the object can reach on a course
to target.

It's obvious that the stronger we shoot up, the higher the object will
reach.  So the question reduces to: “what's the stronger up we can
shoot while still reaching the target?”

In the puzzle's physics system, the vertical velocity decreases by 1
at every turn.  The target is in the negative half of the Y axis
space.  So the key observation is that no matter what strength we
shoot up, the object will always come back to $y=0$ in an integer
number of steps.  What's its Y value one step after that?  Exactly one
below the initial vertical velocity!

So if we shoot upwards with $\left|\max\ y_\mathrm{target}\right|-1
\le v_{y0} \le \left|\min\ y_\mathrm{target}\right| - 1$, we're
guaranteed to hit.  That's not necessarily the only $v_y$ range that
hits, but we can dismiss the others as smaller, so reaching lower.

What about the X coordinate?  In the puzzle, the horizontal velocity
goes one closer to 0 at every step.  So we're able to reach all X
positions of the form $\pm {n(n+1)\over 2}$ in $n$ steps, then stay
there as the horizontal velocity clamps to 0.  So we'd need to verify:

* that the target's width is wider enough than the square root of the
  target's left X coordinate, so that at least one point in the target
  is *reachable*;
* that the square root of the left coordinate is smaller enough than
  double the initial vertical velocity, *a.k.a.* the vertical time to
  reach the target.

Which is the case for my input, and most likely yours too.

That cleared, the answer is a simple summation of all positive $v_y$s
from launch to vertex.

> part1 :: Bounds -> Int
> part1 Bounds{..} = yb * (yb+1) `div` 2

For part 2, we want to count the number of throws that would succeed.
We could write out the math and optimize, but the search space is
small enough that it'd be a waste of engineering time: we'll simply
simulate throws and tally.

> simulate :: Bounds -> (Int,Int) -> Bool
> simulate Bounds{..} = go (0,0) where
>   go (x,y) (vx,vy)
>     | inRange (xl,xr) x && inRange (yb,yt) y = True
>     | vx == 0 && (x < xl || x > xr) = False
>     | y < yb = False
>     | otherwise = go (x+vx,y+vy) (vx - signum vx,vy-1)

We do need cutouts, since the trajectories are unbounded.  I stop
when either:

* horizontal velocity has clamped, and it's not over the target; or
* vertical position has gone beneath the target.

I also need to figure out a bounded set of starting velocities.  We
already have the upper vertical bound from part 1.  A lower trivial
bound is the target's minimum Y itself.  Horizontally, I don't want to
bother identifying which bands hit or miss, so I'll cover the entire
range from $0$ to the target's maximum X.

> part2 :: Bounds -> Int
> part2 Bounds{..} =
>   length $ filter (simulate Bounds{..}) $ range ((0,yb),(xr,-yb-1))

A trivial wrapper sums it up.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

This concludes today's solution.  See you tomorrow!
