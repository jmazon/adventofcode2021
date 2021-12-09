---
title: "AoC Day 6: Lanternfish"
author: Jean-Baptiste Mazon
date: 2021-12-06T12:03:29+01:00
updated: 2021-12-07T01:20:17+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Just implement it, by surprise.
image: aoc-haskell.jpeg
flags: [ mathjax ]
---

<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" type="text/javascript" async></script>

Day 6 of Advent of Code, [“Lanternfish”][aoc], caught me by surprise.
This post is [literate Haskell][gh] as usual, so here's an imports
block to serve as a prologue.

[aoc]: https://adventofcode.com/2021/day/6
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day06.lhs

> import Control.Arrow    ((&&&))
> import Control.Lens
> import Data.List.Split  (wordsBy)
> import Data.Semigroup   (stimesMonoid)
> import Linear.V         (V(V),toVector)
> import Linear.Vector    (zero,basis)
> import Linear.Matrix    ((!*!),(!*),identity,transpose)
> import Data.Vector.Lens (toVectorOf)

The problem describes the individual detail of a lanternfish's
population exponential growth and requests the specific population
count at some point in the future.  Part 1 requests for 80 iterations,
so part 2 will most likely ask for some duration in the order of
magnitude of a billion days, so as to be intractable with a simple
linear approach.

Anyway, let's start simple, we'll expand from there.

Fish with the same internal timer value are indistinguishable, so
let's group them into an array, indexed by timer value.

> type Population = V 9 Int
> parse :: String -> Population
> parse = aggregate . unCsv where
>   unCsv = map read . wordsBy (== ',')
>   aggregate = foldl (\v i -> v & ix i +~ 1) zero

How many fish of each timer index are there after a day?  For most
timer values, it's simply the same number as there were with a timer
one higher.  The two exceptions are:

* timer value 6: in addition to the fish that were at timer value 7,
  this group will also receive the fish who just reproduced, *i.e.*
  the fish that were at timer value 0.
* timer value 8: there's no timer value 9 to collect from, but there's
  the newborn fish to account for.  Those are as numerous as there
  were fish at timer value 0.

This gives the following step function:

> step :: Population -> Population
> step v = other' & _7 +~ breeders where
>   Just (breeders,other) = uncons (toVector v)
>   other' = V (snoc other breeders)

And we get the answer to part 1 with a simple pipeline:

``` Haskell
show . sum . (!! 80) . iterate step . parse
```

We then get to part 2 who makes turns the problem around by requesting
the value for as many as… 256 days.

Oh.

That's going to take, like, more than *three times* longer!  The horror.

> main :: IO ()
> main = interact $ show . ((!! 80) &&& (!! 256)) . map sum . iterate (step) . parse

So be it.

== Upping the Ante

I can't settle for this.  How would we find the answer for a (much)
larger number of days in a reasonable time?

The result would be a crazy big number, unwieldy for a simple web
form.  In a competitive coding environment we'd typically only be
asked for the residue modulo $10^9+7$ or similar large prime.

The `step` function as presented above runs in constant time and
space, even when additionally computing residues.  How can we do
better?

The most common approach is to exploit linearity.  Our step function
is a (linear) endomorphism, so it can be written in matrix notation:

$$
M = \left(\begin{array}{ccccccccc}
0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 \\
0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 \\
0 & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 \\
0 & 0 & 0 & 0 & 1 & 0 & 0 & 0 & 0 \\
0 & 0 & 0 & 0 & 0 & 1 & 0 & 0 & 0 \\
0 & 0 & 0 & 0 & 0 & 0 & 1 & 0 & 0 \\
1 & 0 & 0 & 0 & 0 & 0 & 0 & 1 & 0 \\
0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 1 \\
1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0
\end{array}\right)
$$

> newtype Matrix = M { unM :: V 9 (V 9 Int) }
> m :: Matrix
> m = M (transpose (step <$> V (toVectorOf each basis)))

Our simulation primitive can then be represented as an endomorphism
whose transformation matrix has the form $M^i$, and can be computed in
logarithmic time^[For constant-sized contents.  Which is the case for
mod-$10^9+7$ arithmetic.] using [exponentiation by squaring][ebs].

[ebs]: https://en.wikipedia.org/wiki/Exponentiation_by_squaring

> instance Semigroup Matrix where M a <> M b = M (a !*! b)
> instance Monoid    Matrix where mempty = M identity
> 
> simulate :: Population -> Int -> Population
> simulate start i = unM (stimesMonoid i m) !* start

Let's verify it yields reasonable output.

``` { .repl }
λ> take 20 $ sum <$> iterate step (parse "3,4,3,1,2")
[5,5,6,7,9,10,10,10,10,11,12,15,17,19,20,20,21,22,26,29]

λ> take 20 $ sum <$> map (simulate (parse "3,4,3,1,2")) [0..]
[5,5,6,7,9,10,10,10,10,11,12,15,17,19,20,20,21,22,26,29]

λ> find (uncurry (/=)) $
     zip (iterate step (parse "3,4,3,1,2"))
         (map (simulate (parse "3,4,3,1,2")) [0..])
<<time passes>>
Interrupted.
```

Looks good to me.

A final warning.  I'm using `stimesMonoid` to skip re-implementing
exponentiation by squaring yet again.  You may recall [`Endo`][endo]
is an instance of Monoid too.  Why am I not simply using `stimesMonoid
i (Endo step)` to skip the matrix stuff altogether?  The short answer
is that though it would indeed yield the correct results, it wouldn't
do so in the targetted timeframe.  Can you spot why?

[endo]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Monoid.html#t:Endo

This concludes today's solution.  See you tomorrow!
