---
title: "AoC Day 18: Snailfish"
author: Jean-Baptiste Mazon
date: 2021-12-18T18:40:15+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Lenses and zippers for explodability exploitation
image: aoc-haskell.jpeg
---

Today in Advent of Code, the day 18 puzzle [“Snailfish”][aoc], asks us
to implement a borderline imperative numeric system on binary trees of
digits. This post is [literate Haskell][gh] with a big import list as
I'll be leveraging quite a few libraries here.

[aoc]: https://adventofcode.com/2021/day/18
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day18.lhs

> import Control.Arrow             ((&&&),second)
> import Control.Lens
> import Control.Zipper
> import Control.Zipper.Internal   (focalPoint)
> import Control.Monad             ((>=>))
> import Control.Monad.Trans.Accum (Accum,runAccum,add,looks)
> import Data.Char                 (isDigit,digitToInt)
> import Data.Data                 (Data)
> import Data.Maybe                (fromMaybe,listToMaybe)
> import Data.Monoid               (First(First,getFirst))
> import Text.Megaparsec           (Parsec,parse,satisfy,(<|>))
> import Text.Megaparsec.Char      (char)

Snailfish numbers are a binary tree of integers.  The integers are in
the digit range and the tree's depth is always between 1 and 4, but I
won't enforce that in the type as it'll make my life easier when
performing the operations.^[I *would* be curious to see what others
have done in this regard.]  I'll be using the derived traversable hack
again, so you can read `a` as `Int` for all intents and purposes.

> data Number a = Regular !a | Pair !(Number a) !(Number a)
>   deriving (Functor,Foldable,Traversable,Data)
> instance Data a => Plated (Number a)
> makePrisms ''Number

To add two numbers, form a pair from them then reduce it.

> addNumbers :: Number Int -> Number Int -> Number Int
> addNumbers = (reduce .) . Pair

Reduction is repeated application of the first operation that applies,
until none does anymore.  The operations are:

1. exploding the leftmost 4-deep pair
2. splitting the leftmost superdigit number

> reduce :: Number Int -> Number Int
> reduce n = fromMaybe n (reduce <$> explodePair n <|> reduce <$> splitRegular n)

I'll define two functions to detect and perform those operations,
returning `Nothing` if they had nothing to do.

Exploding a pair involves replacing it with 0, which is easy, then
adding its former constituent digits to the closest digit on either
side, which isn't strictly hard, but clearly a bad match for pure
functional programming.

I'll resolve the mismatch by making a first use of the derived
`Functor` instance: I'll have the localizing piece of code replace
every digit in the tree with `Just` that digit, replace the leftmost
explodable pair with `Nothing` and return its former contents.  If
there's no exploitable leftmost pair, the function returns `Nothing`
instead.

> zapAndReturnPair :: Number Int
>                  -> Maybe ((Int,Int),(Number (Maybe Int)))
> zapAndReturnPair n = do
>   z <- zipper (Just <$> n) &
>        withins plate >>= withins plate >>= withins plate >>= withins plate >>=
>        withins _Pair &
>        listToMaybe
>   let (Regular (Just a),Regular (Just b)) = z ^. focus
>   z & upward & focus .~ Regular Nothing & rezip
>     & (,) (a,b) & pure

I'm using `withins` calls in the list monad and converting the
resulting zipper list to `Maybe` to achieve the effect of finding the
leftmost.  Performing the `withins` directly in `Maybe` would have
“and” semantics, and fail at leftmosting.^[I'm actually not totally
clear in my mind whether it would only accept a leftmost in first
position or a leftmost in first position only if all candidate
positions are pairs.  Too lazy to explore at this point.]

With the tree of `Just` digits and a single `Nothing`, I can now zip
using the autoderived `Traversable` instance.  It has the nice
flattening property of ordering slots left-to-right we're after to
seek neighbors.  I'll skip to the `Nothing`, add `a` to its left, `b`
to its right, then rezip and eliminate the internal `Maybe`.

> propagateAroundNothing :: (Int,Int) -> Number (Maybe Int) -> Maybe (Number Int)
> propagateAroundNothing (a,b) n = zipper n
>    &  within traversed
>   >>= forwardToNothing
>   <&> a `addedAt` leftward
>   <&> b `addedAt` rightward
>   <&> rezip
>   <&> fmap (fromMaybe 0)

This uses two simple helpers.  One to seek the `Nothing` element with
the new, flat traversal model:

> forwardToNothing :: MonadFail m => Zipper h i (Maybe a) -> m (Zipper h i (Maybe a))
> forwardToNothing z
>   | has (focus . _Nothing) z = pure z
>   | Just z' <- rightward z = forwardToNothing z'
>   | otherwise = fail "forwardToNothing: no Nothing"

And one to add an integer value some zipper path away from the focus:

> addedAt :: Functor f
>         => Int -> (Zipper h i (f Int) -> Maybe (Zipper h i (f Int)))
>         -> Zipper h i (f Int) -> Zipper h i (f Int)
> addedAt n dir z = dir z <&> focus %~ fmap (+n) >>= moveTo i & fromMaybe z
>   where i = focalPoint z

We've got all we need to combine our pair explosion function!

> explodePair :: Number Int -> Maybe (Number Int)
> explodePair = zapAndReturnPair >=> uncurry propagateAroundNothing

The second detect-and-apply function we need is the one to split a
number greater than 9.  The autogenerated `Plated` instance I have for
`Number` is great for transforming each structural recursive point,
but I only want to operate on the leftmost one.  And to detect whether
the operation was necessary.

So I'll run the traversal in an `Accum` monad.

> splitRegular :: Number Int -> Maybe (Number Int)
> splitRegular n =
>   uncurry (<$) $ second getFirst $
>   runAccum (transformM splitTransformer n) (First Nothing)

Yay.  I unironically wrote “`second getFirst`”.

The local transformer keeps track of the accumulator to only perform
the replacement once, and degrade as `id` the rest of the time.

> splitTransformer :: Number Int -> Accum (First ()) (Number Int)
> splitTransformer (Regular n) = looks getFirst >>= \case
>   Nothing | n >= 10 ->
>     Pair (Regular (n `div` 2)) (Regular ((n+1) `div` 2))
>       <$ add (First (Just ()))
>   _ -> pure (Regular n) 
> splitTransformer x = pure x

I still need to parse the input.  It's bordering “trivial” on the
relative scale of what we've been through this month.

> type Parser = Parsec () String
> number,pair,regular :: Parser (Number Int)
> number = pair <|> regular
> pair = Pair <$> (char '[' *> number) <* char ',' <*> number <* char ']'
> regular = Regular . digitToInt <$> satisfy isDigit

Part 1 asks for a checksum of the sum of all numbers in the input.
That checksum is dubbed “magnitude” and follows a simple recursive
definition.

> magnitude :: Number Int -> Int
> magnitude (Regular n) = n
> magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b
>
> part1 :: [Number Int] -> Int
> part1 = magnitude . foldl1 addNumbers

Part 2 asks for the largest pairwise sum we can find.

> part2 :: [Number Int] -> Int
> part2 ns = maximum $ map magnitude (addNumbers <$> ns <*> ns)

A small wrapper and we're all done!

> main :: IO ()
> main = interact $ show . fmap (part1 &&& part2) .
>                   mapM (parse number "<stdin>") . lines

This concludes today's solution.  See you tomorrow!
