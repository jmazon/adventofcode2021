---
title: "AoC Day 21: Dirac Dice"
author: Jean-Baptiste Mazon
date: 2021-12-21T11:42:50+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: When you can't think of anything else, use free monads
image: aoc-haskell.jpeg
---

Today's Advent of Code problem, [“Dirac Dice”][aoc], is the one I
least like talking about.  I did enjoy solving it.  The twist from
part 1 to part 2 *is* good.  Simply put, my issue with it is that
despite all my efforts, there's so little code to share between both
parts it all feels a bit artificial.  So bear with me, it's still a
[literate Haskell][gh] post with imports on top.

[aoc]: https://adventofcode.com/2021/day/21
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day21.lhs

> import Control.Arrow ((&&&))
> import Control.Lens
> import Control.Monad.Cont
> import Control.Monad.Free
> import Control.Monad.Free.TH
> import Control.Monad.Trans.Accum
> import Data.Function (fix)
> import Data.Monoid (Sum(Sum))
> import Data.MultiSet (MultiSet)
> import qualified Data.MultiSet as MSet

The problem state is very simple: two players, each with a position
and a score.  One of those has the distinction of being active.

> data State = State
>   { _currentPlayer :: !Player
>   , _nextPlayer    :: !Player
>   }
>   deriving (Eq,Ord)
> 
> data Player = Player
>   { _plId  :: !PlayerId
>   , _pos   :: !Int
>   , _score :: !Int
>   }
>   deriving (Eq,Ord)
> 
> data PlayerId = P1 | P2 deriving (Eq,Ord)
>
> makeLenses ''State
> makeLenses ''Player

The game operation is rather simple.  I'll write it up in a free
monad.

> data GameF a = Roll (Int -> a) | End State deriving Functor
> makeFree ''GameF
> 
> step :: MonadFree GameF m => Int -> State -> m State
> step lim st = do
>   r1 <- roll
>   r2 <- roll
>   r3 <- roll
>   let pl' = advance (r1 + r2 + r3) (st ^. currentPlayer)
>       st' = State { _currentPlayer = st ^. nextPlayer
>                   , _nextPlayer = pl'
>                   }
>   if pl' ^. score >= lim then end st' else pure st'

The step function takes a `lim` argument to limit the game to reaching
a specific score.  It then rolls the die three times, advances the
player, adds their resulting position to their score and returns the
state.  Either as a simple monadic value when the game is still live,
or as a terminal free member.

The entire tricky part of the implementation is in this helper, that
concentrates all the off-by-one errors you could dream of in a single
place.

> advance :: Int -> Player -> Player
> advance n pl =
>   let pos' = 1 + (pl ^. pos - 1 + n) `mod` 10
>   in pl & pos .~ pos'
>         & score +~ pos'

Part 1 is the training game: it stops at a score of 1 000, the die is
predictable and we are asked for a specific checksum involving the
loser's score.

I'll implement the die throw at little cost using my very good friend
the `Accum` monad.

> type Training = Accum (Sum Int)
> 
> rollTraining :: Training Int
> rollTraining = do
>   Sum n <- look
>   add (Sum 1)
>   pure (n `mod` 100 + 1)

This has the advantage of easily letting me read the roll count in the
end.

Then I can write a wrapper to interpret the game and return the
requested checksum.

> runTraining :: State -> Int
> runTraining st0 = loserScore * rolls where
>   (loserScore,Sum rolls) =
>     runAccum (runContT (callCC (\end -> iterM (interpret end) (game st0))) pure)
>              mempty
>   game = fix $ \loop st -> step 1000 st >>= loop
>   interpret end = \case
>     Roll next -> lift rollTraining >>= next
>     End st -> end (st ^. currentPlayer . score)

I'm not too satisfied with resorting to `callCC` to extract the
result, but the types of both `foldFree` and `iterM` didn't appear to
let me do otherwise.  Free monad experts come to me!

Anyway, this runs fine and dandy and earns a star.

Now to part 2.  Here rolling the die splits the universe.  “I know”, I
think, “I'll use a list monad!”

> type Dirac = []
>
> rollDirac :: Dirac Int
> rollDirac = [1..3]

Well, Haskell makes it easy to split the world, but that doesn't make
it the right thing to do.  The example in the problem statement is a
good enough hint: if we're going to count victories one by one in
addition to all the consing, it won't be done in time for next
Christmas.

The seasoned competitive programmers will have noticed that the core
space is quite small, and we're limiting Dirac games to only a few
*scores*, so it's a rather direct application of [dynamic
programming][dp].

[dp]: https://en.wikipedia.org/wiki/Dynamic_programming

But DP isn't strictly necessary either.  We can use the same kind of
trick as on [day 14][14]: the state cardinality is smaller than $2
\times 10^2 \times 21^2 = 88\,200$.  By flattening it after each step,
we only have that times the $3^3$ Dirac splits to explore per turn.
And the maximum turn count is very trivially bounded (and small).

[14]: /posts/2021-12-aoc/day14.html

That path does introduce the problem of counting victories.  It would
be a bit unwieldy to thread a Writer monad through: the tallies do
have to be multiplied by the occurrence count, which isn't trivially
accessible at free monad interpreter level.  I chose to expand the
state with a `Nothing` value, that will represent whatever end state
we want to count.  So the game step function just needs to ignore (but
keep) them:

> type State' = Maybe State
> 
> diracStep :: MultiSet State' -> MultiSet State'
> diracStep = MSet.unionsMap $
>   MSet.fromList . maybe [Nothing] (iterM interpretDirac . fmap Just . step 21)

The interpreter isn't much more complicated than the previous one.
The `End` case is where we convert a victory to a tally.

> interpretDirac :: GameF (Dirac State') -> Dirac State'
> interpretDirac (Roll next) = rollDirac >>= next
> interpretDirac (End st) = Nothing <$ guard (st ^. nextPlayer . plId == P1)

The Dirac runner is actually simpler.  Thank the sane checksum.

> runDirac :: State -> Int
> runDirac = MSet.size . stabilize diracStep . MSet.singleton . Just

Here's the rest of the code for completeness.^[You don't really think
I wrote a parser during Advent for just two digits?]

> stabilize :: Eq a => (a -> a) -> a -> a
> stabilize f = go where
>   go x | x' == x   = x
>        | otherwise = go x'
>     where x' = f x
> 
> parse :: String -> State
> parse (map (read . last . words) .  lines -> [a,b]) =
>   State { _currentPlayer = Player { _plId = P1, _pos = a, _score = 0 }
>         , _nextPlayer    = Player { _plId = P2, _pos = b, _score = 0 }
>         }
> 
> main :: IO ()
> main = interact $ show . (runTraining &&& runDirac) . parse

This concludes today's solution.  I hope you enjoyed it, because in
all honesty I only wrote it for completeness.  See you tomorrow!
