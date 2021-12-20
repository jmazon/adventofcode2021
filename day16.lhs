---
title: "AoC Day 16: Packet Decoder"
author: Jean-Baptiste Mazon
date: 2021-12-16T12:39:32+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: A two-stage parser/evaluator
image: aoc-haskell.jpeg
---

The Advent of Code puzzle of the day, [“Packet Decoder”][aoc], is a
weird blend of parsing and evaluation.  This post is [literate
Haskell][gh]; I've kept it short on the imports, you'll thank me
later.

[aoc]: https://adventofcode.com/2021/day/16
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day16.lhs

> import Control.Arrow ((&&&),first)
> import Control.Monad (replicateM)
> import Data.Char     (digitToInt)
> import Text.Megaparsec

Yeah, Megaparsec.  I really didn't go out for complexity here.

Okay, so the puzzle explicitly gives us the hexadecimal translation
table: I won't resist it.  Here it is again, copy and pasted and after
a few Emacs macros' worth of processing.

> deHex :: Char -> String
> deHex '0' = "0000"
> deHex '1' = "0001"
> deHex '2' = "0010"
> deHex '3' = "0011"
> deHex '4' = "0100"
> deHex '5' = "0101"
> deHex '6' = "0110"
> deHex '7' = "0111"
> deHex '8' = "1000"
> deHex '9' = "1001"
> deHex 'A' = "1010"
> deHex 'B' = "1011"
> deHex 'C' = "1100"
> deHex 'D' = "1101"
> deHex 'E' = "1110"
> deHex 'F' = "1111"
> deHex  _  =   ""

That last line is just there to make my life easier around the
possibility of there being a newline at the end.

I write the output as `Char`s, but the `main` wrapper will map it to
`Bool`s, so assume that's what we have down the pipe.

Next up, the puzzle's datatypes.  We've got a packet with a version
we'll want to sum across the data structure.  I'll make it a type
parameter and use the good old autoderived Foldable.  So read `a` as
`Int`.

> data Packet a = Packet !a !(Payload a) deriving Foldable

Then the payload.  As far as part 1 goes, all we need to parse are
literals and a broad “operator” type with nested packets.  Hence the
`a` type parameter reaching over to the payload.

> data Payload a = Literal !Int | Operator !Op [Packet a] deriving Foldable

Now the packet header is a straightforward decoding of fixed-width
integers.  The rest of the parsing is delegated to a specific parser.
The power of monadic[^monadic] parser combinators…

[^monadic]: All parsers presented here could really be implemented as
applicatives, but the syntax is just a bit too hairy.  Cue
ApplicativeDo comments.

> type Parser = Parsec () [Bool]
>
> packet :: Parser (Packet Int)
> packet = do
>   v <- binary 3
>   t <- binary 3
>   Packet v <$> if t == 4 then literal else operator t

Parsing a single fixed-width integer is a simple matter of reading
said width through and converting it.

> binary :: Int -> Parser Int
> binary n = readBin <$> replicateM n anySingle

Binary conversion is your good old fold.  As a toplevel function
because I'll re-use it shortly.

> readBin :: [Bool] -> Int
> readBin = foldl (\a b -> 2*a + fromEnum b) 0

One of the two specific payload parsers is the one for literals.  It
parses groups of 5 bits, ending on one that leads with a zero.  The
middle line is a bit weirder than I'd like, it's just there to patch
up the results from `manyTill_` point-freely.^[Not that point-free
style really brings legibility here.  It's AoC, I'm having fun.]

> literal :: Parser (Payload Int)
> literal = Literal . readBin .
>           uncurry (++) . first concat <$>
>           manyTill_ (group True) (group False)
>   where group c = single c *> replicateM 4 anySingle

Decoding is provided for by the same `readBin` function as before.

Operator parsing is slightly trickier.  There are two cases to
disambiguate.

* We could be parsing any number of packets that fit within a fixed
  width.
* We could be parsing a given number of packets.

> operator :: Int -> Parser (Payload Int)
> operator o = Operator (toEnum o) <$> do
>   anySingle >>= \case
>     False -> do
>       l <- binary 15
>       recurse l (many packet)
>     True -> do
>       n <- binary 11
>       replicateM n packet

The `recurse` combinator… I expected to find directly in Megaparsec,
but didn't.  So here's an interpretation of it.^[And that one doesn't
cut it as an applicative.  But it's a combinator, I hope the
conceptual purity gods will forgive me.]

> recurse :: Int -> Parser a -> Parser a
> recurse l parser = do
>   (i,i') <- splitAt l <$> getInput
>   setInput i
>   r <- parser
>   setInput i'
>   pure r

For part 2, we've got to actually implement the operations.  Here are
the recognized types.

> data Op = OpSum | OpProd | OpMin | OpMax | OpLit | OpGT | OpLT | OpEq deriving Enum

A simple recursive evaluation function does the grunt work.

> eval :: Packet Int -> Int
> eval (Packet _ pl) = case pl of
>   Literal n -> n
>   Operator OpSum  ps  -> sum      (eval <$> ps)
>   Operator OpProd ps  -> product  (eval <$> ps)
>   Operator OpMin  ps  -> minimum  (eval <$> ps)
>   Operator OpMax  ps  -> maximum  (eval <$> ps)
>   Operator OpGT [a,b] -> fromEnum (eval a  > eval b)
>   Operator OpLT [a,b] -> fromEnum (eval a  < eval b)
>   Operator OpEq [a,b] -> fromEnum (eval a == eval b)

In normal code, not split in two parts as on AoC, I'd have the parser
check those last three cases for the proper number of subpackets, so
it wouldn't take until so late to discover a relational operator has
the wrong number of arguments.  But that's what we get for split
goals.

The `main` wrapper is more interesting than usual: notice how part 1
reduces to a simple `sum` call on the tree.

> main :: IO ()
> main = interact $ show .
>                   fmap (sum &&& eval) .
>                   parse packet "<stdin>" .
>                   map (toEnum . digitToInt) .
>                   concatMap deHex

This concludes today's solution.  See you tomorrow!
