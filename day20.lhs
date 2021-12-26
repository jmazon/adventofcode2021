---
title: "AoC Day 20: Trench Map"
author: Jean-Baptiste Mazon
date: 2021-12-20T11:10:50+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Infinite flashing warning
image: aoc-haskell.jpeg
---

Earlier this month Eric Wastl tweeted along the lines of “I love your
visualizations, but please include epilepsy warnings when appropriate.
He must have had today's problem, [“Trench Map”][aoc], in mind at the
time.  This post is [literate Haskell][gh], here are a few imports.

[aoc]: https://adventofcode.com/2021/day/20
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day20.lhs

> import Control.Arrow ((&&&))
> import Data.Array    (Array,listArray,(!))
> import Data.Set      (Set)
> import qualified Data.Set as Set

The problem's core difficulty is handling the infinite surface.  As is
common for such problems, I'll represent the image as the set of lit
points.

> type Pos = (Int,Int)
> 
> parse :: String -> (Array Int Bool,Set Pos)
> parse (lines -> (p:"":g)) =
>   ( listArray (0,511) (map (== '#') p)
>   , Set.fromList [ (i,j) | (i,l) <- zip [0..] g, (j,'#') <- zip [0..] l ]
>   )

The points not in the set are thus assumed to be unlit.

But everybody's problem input comes with a twist: the great void areas
not in the set all light up on turn one.  So we'll need to be smarter.
We'll keep the set of pixels idiom, but refine it so that it can
alternatively refer to the set of unlit pixels, the other being
assumed to be lit.  How do we tell them apart?  I'll package in a
read-write modifier function, that'll automatically perform the
decoding.

> data Picture = Picture
>   { picPixels :: !(Set Pos)
>   , picStep   :: !(Array Int Bool)
>   , picRW     :: [Bool -> Bool]
>   }

I store it as a list since all future versions of it can be known from
the start.  I also package in the transition table.  Here's what the
accessors look like:

> picRead :: Picture -> Pos -> Bool
> picRead Picture{picPixels = pix,picRW = r:_} p = r (Set.member p pix)
> 
> picWrite :: Picture -> Bool -> Bool
> picWrite Picture{picRW = _ : w : _} v = w v

The only tricky aspect of it is that the flow of the program later on
will be to read step $N$ to write step $N+1$, so the write modifier is
offset.

Now, how do I predict what the modifier function should be?  Well, it
depends on the transition table.  The picture starts as mostly unlit
(a finite portion of it is lit), so the read modifier starts as a
no-op.  What happens after a step?  The finite portion may expand a
bit, but most of the pixels are the infinite area that started out as
zeros.  What do those become?

It depends.  If bit 0 of the transition table is unset, they remain as
zeros, so our representation is stable.  If it is one, the infinite
section flips on first transition, so the new accessor should be a
reversing lens.  But what happens on the *next* transition?

It depends again.  If bit 511 of the transition table is set, the
infinite area of the picture remains lit, so the representation is
actually stable too, just not to the same state as it was given.  If
it's unset, the infinity flips again and we're back in the same
situation as the start.

So we can analyze and package a `Picture` as 3 cases:

> analyze :: Array Int Bool -> Set Pos -> Picture
> analyze a s = Picture s a $ case (a!0,a!511) of
>                               (False,_)    -> repeat id
>                               (True,True)  -> id : repeat not
>                               (True,False) -> cycle [id,not]

Now the representation is covered, let's tackle the step function.
The modifier is simply shifted to the next one in the list, as set at
analysis time.  The transition function remains stable, that's what we
introduced the modifier for.  The picture remains to be addressed.

The infinite part of it is covered by default: it's either all zeros
or all ones, but it's not a part of the resulting set.  Let's consider
the interesting pixels instead: those that are explicitly referred to.

It's not enough to consider previously interesting pixels as being
interesting for the next iteration: given a neighborhood-based
transition matrix, any pixel adjacent to an interesting pixel could
become interesting.

So let's compute the transition boundary for the picture's
representation, found by exploring all adjacent pixels to previously
interesting pixels, including themselves.

> neighborhood :: Pos -> [Pos]
> neighborhood (i,j) = [ (i-1,j-1), (i-1,j), (i-1,j+1)
>                      , (i  ,j-1), (i,  j), (i,  j+1)
>                      , (i+1,j-1), (i+1,j), (i+1,j+1)
>                      ]
> 
> boundary :: Set Pos -> Set Pos
> boundary = Set.unions . Set.map (Set.fromList . neighborhood)

The stepping function is then a simple matter of looking up a pixel's
neighborhood in the transition table.

> step :: Picture -> Picture
> step p = Picture
>   { picPixels = Set.fromList
>       [ (i,j)
>       | (i,j) <- Set.elems (boundary (picPixels p))
>       , let bs = map (picRead p) (neighborhood (i,j))
>       , picWrite p (picStep p ! foldl (\a b -> 2*a + fromEnum b) 0 bs)
>       ]
>   , picStep = picStep p
>   , picRW = tail (picRW p)
>   }

A little wrapper can then count interesting pixels after steps 2
and 50.  Note that I don't need to wonder whether the statement asks
for lit or unlit pixels: if it wants an answer, it's got to be the
finite set, whichever it is at the moment.

> main :: IO ()
> main = interact $
>          show . ((!! 2) &&& (!! 50)) . map (Set.size . picPixels) .
>          iterate step . uncurry analyze . parse 

This concludes today's solution.  See you tomorrow!
