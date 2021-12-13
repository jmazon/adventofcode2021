---
title: "AoC Day 12: Passage Pathing"
author: Jean-Baptiste Mazon
date: 2021-12-12T19:16:33+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: It's all DFS when you squint
image: aoc-haskell.jpeg
---

For day 12's puzzle, [“Passage Pathing”][aoc], we'll be exploring a
cave network.  Here are a few imports to start the [literate
Haskell][gh] with:

[aoc]: https://adventofcode.com/2021/day/12
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day12.lhs

> import Control.Arrow   ((&&&))
> import Control.Monad.Writer.Strict
> import Data.Char       (isUpper)
> import Data.Map.Strict (Map,(!))
> import Data.Proxy      (Proxy(Proxy))
> import Data.Set        (Set,(\\))
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set

Caves are provided with a nice human-readable label we can use to
refer to them.

> type CaveId = String

Some caves are small; some are big.  Tell them apart using this neat
trick:

> isBigCave :: CaveId -> Bool
> isBigCave = all isUpper

I'll represent the network topology with standard containers: for each
cave I'll map a set of reachable caves, all referenced by label.

> type Graph = Map CaveId (Set CaveId)

Reading all this information in is just a simple matter of connecting
various combinators together.

> parse :: String -> Graph
> parse = oneWay . fmap noReturn .
>         foldr (uncurry (Map.insertWith Set.union)) Map.empty .
>         concatMap readEdge . lines
>   where
>     readEdge (break (== '-') -> (a,'-':b)) =
>       [(a,Set.singleton b),(b,Set.singleton a)]
>     oneWay = Map.insert "end" Set.empty
>     noReturn = Set.delete "start"

The `readEdge` internal function takes charge of making edges
bidirectional.  Later in the statement, the two edges `start` and
`end` are given special treatment by being declared source-only and
sink-only.  I handle that directly at parse time by post-processing
the adjacency lists to not allow moving back to `start`, and adjusting
the graph not to be able to leave `end`.

We can now write out a fairly generic DFS that specifically performs a
tallying operation when reaching a final node:

> dfs :: forall s. Node s => Proxy s -> Graph -> Sum Int
> dfs _ g = execWriter (go (start @s)) where
>   go n = when (isFinal n) (tell (Sum 1)) *>
>          mapM_ go (visit g n)

It's so generic we're to bring it our own node class.

> class Node s where
>   start :: s
>   isFinal :: s -> Bool
>   visit :: Graph -> s -> [s]

In part 1 the movement rules are to never visit a small cave twice.
We can keep track of this by remembering which small caves we've
visited.

> data Part1 = P1 CaveId (Set CaveId) deriving Show

The `Node` instance is then a typical DFS's, with the small
specificity that big caves are ignored when managing the closed set.

> instance Node Part1 where
>   start = P1 "start" Set.empty
>   isFinal (P1 c _) = c == "end"
>   visit g (P1 c cl) = map (\c' -> P1 c' cl') (Set.elems cs)
>     where cl' | isBigCave c = cl
>               | otherwise = Set.insert c cl
>           cs = g!c \\ cl

In part 2, we're allowed to revisit a single small cave once.  We can
handle this with minimal change by tracking an additional boolean
value: “have we visited a small cave twice already?”

> data Part2 = P2 CaveId Bool (Set CaveId) deriving Show

The `start` and `isFinal` methods are simple adjustment to the added
parameter in the data structure.

> instance Node Part2 where
>   start = P2 "start" False Set.empty
>   isFinal (P2 c _ _) = c == "end"

(It's also where we “explicitly” accept reaching the end cave with or
without having visited a small cave twice.)

The `visit` method will have to be altered:

* The list of neighbors to visit from a cave now has two
  possibilities: before visiting a small cave twice we move
  unrestrictedly anywhere; afterwards we subtract the closed set as in
  part 1.
* The closed set handling is where we'll detect we're visiting a small
  cave for the second time.

>   visit g (P2 c db cl) = map (\c' -> P2 c' db' cl') (Set.elems cs)
>     where (db',cl') | isBigCave c = (db,cl)
>                     | c `Set.member` cl = (True,cl)
>                     | otherwise = (db,Set.insert c cl)
>           cs | db' = g!c \\ cl
>              | otherwise = g!c

A simple wrapper dispatches it all.

> main :: IO ()
> main = interact $ show . (dfs (Proxy @Part1) &&& dfs (Proxy @Part2)) . parse

This concludes today's solution.  See you tomorrow!
