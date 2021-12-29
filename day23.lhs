---
title: "AoC Day 23: Amphipod"
author: Jean-Baptiste Mazon
date: 2021-12-23T23:14:45-01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Yo, pawd! I put search in your search!
image: aoc-haskell.jpeg
---

Two days before the end of Advent of Code, the day 23
[“Amphibot”][aoc] puzzle has us optimize colored pawn movements on a
constrained grid.  As this post is [literate Haskell][gh], it starts
with a bunch of imports.

[aoc]: https://adventofcode.com/2021/day/23
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day23.lhs

> import           Data.Array
> import           Data.Char       (isAlpha)
> import           Data.Maybe      (mapMaybe)
> import           Data.Map.Strict (Map)
> import qualified Data.Map.Strict as M
> import qualified Data.Set as S

Amphipods come in four colors: A, B, C and D.

> data Amphipod = Amber | Bronze | Copper | Desert deriving (Eq,Ord)

Each has a specific movement cost.

> mult :: Amphipod -> Int
> mult Amber  = 1
> mult Bronze = 10
> mult Copper = 100
> mult Desert = 1000

The grid is either wall or walkable.  The space outside the grid is
needed to parse to a rectangular array, but isn't reachable in any way
so it doesn't matter too much what its type is.  Among the walkable
areas, the starting ones are “rooms”, the ones directly above are
technically hallway, but forbidden to stop on, and the remainder is
genuine “hallway”.

> data CellType = Hallway | Forbidden | Room Amphipod | Wall deriving Eq
> type Pos = (Int,Int)
> type Grid = Array Pos CellType

Now the authorized movements are very constrained.  Amphipods start in
rooms, mostly shuffled around.  From a room, they can only move to a
hallway.  From a hallway, they can only move to the room they're
supposed to end in.  So for all intents and purposes, each can really
only move twice ever: any additional intermediate position is either
forbidden or suboptimal, which is as good as forbidden for search.

And the solution will have at most twice the number of amphipods as
moves.  So we can most likely use a simple shortest path algorithm
such as [Dijkstra's][ijk].

[ijk]: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

> dijkstra :: Ord a => a -> (a -> Bool) -> (a -> [(Int,a)]) -> Int
> dijkstra startNode isGoalNode expandNode =
>     go S.empty (S.singleton (0,startNode))
>   where
>     go cl (S.deleteFindMin -> ((d,n),q))
>       | n `S.member` cl = go cl q
>       | isGoalNode n = d
>       | otherwise = go cl' q'
>       where
>         cl' = S.insert n cl
>         q' = foldl (flip S.insert) q $ map (\(d',n') -> (d+d',n')) (expandNode n)

To solve, we simply call it with the three relevant arguments.

There are multiple moving pieces: they all have to be a part of the
state.

> type State = Map Pos Amphipod

The goal position is unique so long as the amphipods are
indistinguishable per color.  Which is the case with the state
representation I chose.  So I can just generate it once and compare
for equality.

> solve :: Grid -> State -> Int -> Int
> solve g s0 roomDepth = dijkstra s0 (== goal) expand where
>   goal = M.fromList $ mapMaybe toGoal $ assocs g where
>     toGoal (p,Room a) = Just (p,a)
>     toGoal   _        = Nothing

The node expansion is obviously where all the nitty gritty is going.
At its core, the node expansions are the sum of each amphibot's
expansions.

>   expand n = concatMap (expandNode n) (M.assocs n)

An individual amphipod's expansion spans longer.  For each of them, we
want to:

* check which positions it can reach (with a [DFS][dfs])
* check whether it's a valid movement: room to hallway, hallway to
  final room only
* prune a forbidden move: blocking an unaligned amphipod
* prune the stupid move: leaving extra space at the end of the room.
  It's not forbidden by the statement *per se*, but with my model of
  only out and in moves it would make the game unfinishable, better
  trim it early.

[dfs]: https://en.wikipedia.org/wiki/Depth-first_search

>   expandNode n (p,a) =
>       map moveTo $ filter (not . blocks . fst) $ filter (checkType . fst) $ dfs n p
>     where
>       t = g!p
>       checkType p' = case (t,g!p') of
>         (Room _,Hallway)            -> True
>         (Hallway,Room a') | a' == a -> True
>         _ -> False

The `blocks` function is a bit too introspective for my taste, but
handling coordinates directly does make checking the room contents for
empty or “enemies” easy.  `i > 2` mostly means “is a room”.

>       blocks :: Pos -> Bool
>       blocks (i,j)
>         | i > 2 = any (/= a) (mapMaybe (\i' -> M.lookup (i',j) n)
>                                [2+1..2+roomDepth])
>                || any (\i' -> (i',j) `M.notMember` n) [i+1 .. 2+roomDepth]
>       blocks _ = False

With all those checks passed, we can generate the resulting node by
simple set operations.

>       moveTo (p',c) = (c * mult a,M.insert p' a (M.delete p n))

Two helpers are needed.  One for listing each position's walkable
direct neighbors.

>   neighbors :: Array Pos [Pos]
>   neighbors = array (bounds g)
>     [ (p,[ p'
>          | p' <- [(i-1,j),(i,j+1),(i+1,j),(i,j-1)]
>          , inRange (bounds g) p'
>          , g!p' /= Wall
>          ])
>     | p@(i,j) <- indices g
>     ]

And one for generating a position's complete walkable span.  Including
places where we can't stop, those were filtered in the `checkTypes`
function.

>   dfs :: Map Pos Amphipod -> Pos -> [(Pos,Int)]
>   dfs n p0 = go S.empty [(p0,0)] where
>     go _ [] = []
>     go cl ((p,d):q)
>       | p `S.member` cl = go cl q
>       | otherwise = (p,d) : go cl' (q' ++ q)
>       where
>         cl' = S.insert p cl
>         q' = map (,d+1) $ filter (`S.notMember` cl) $
>              filter (`M.notMember` n) $ neighbors ! p

My parsing code uses the picture to generate cell types, but still
relies heavily on outside information (room color) and constants
(forbidden positions' coordinates).

> parse :: [String] -> (Grid,State)
> parse ls = (grid,state) where
>   h = length ls
>   w = length (head ls)
>   grid = accumArray (flip const) Forbidden ((1,1),(h,w))
>          [ ((i,j),parseType j c)
>          | (i,l) <- zip [1..] ls
>          , (j,c) <- zip [1..] l
>          ]
>   parseType _ '#' = Wall
>   parseType j '.' | even j && j > 3 && j < 11 = Forbidden
>                   | otherwise = Hallway
>   parseType j c | isAlpha c = case j of
>                                 4  -> Room Amber
>                                 6  -> Room Bronze
>                                 8  -> Room Copper
>                                 10 -> Room Desert
>   parseType _ _ = Wall
>   state = M.fromList
>           [ ((i,j),a)
>           | (i,l) <- zip [1..] ls
>           , (j,c) <- zip [1..] l
>           , a <- parseAmphipod c
>           ]
>   parseAmphipod 'A' = [Amber]
>   parseAmphipod 'B' = [Bronze]
>   parseAmphipod 'C' = [Copper]
>   parseAmphipod 'D' = [Desert]
>   parseAmphipod _ = []

For part 2, a constant piece of paper is to be spliced in the middle.

> patchedLines :: [String] -> [String]
> patchedLines (splitAt 3 -> (start,end)) = start ++ merge ++ end
>   where merge = [ "  #D#C#B#A#", "  #D#B#A#C#" ]

Here's a little `main` wrapper for completeness.

> main :: IO ()
> main = do
>   ls0 <- lines <$> getContents
>   let (g,s0) = parse ls0
>   print (solve g s0 2)
>   let ls0' = patchedLines ls0
>       (g',s0') = parse ls0'
>   print (solve g' s0' 4)

There's a lot of optimization that could be added.  More and better
pruning, A* search, bidirectional search, you name it.  But it runs in
ten seconds with mostly boilerplate code, I'm not sure it's worth
any more.  The engineering tradeoff strikes again…

This concludes today's solution.  See you tomorrow!
