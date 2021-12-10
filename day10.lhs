---
title: "AoC Day 10: Syntax Scoring"
author: Jean-Baptiste Mazon
date: 2021-12-10T15:42:50+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: Abuse of parsing libraries
image: aoc-haskell.jpeg
---

Advent of Code day 10 [“Syntax Scoring”][aoc] provides us with invalid
parenthesized expressions and asks us for a convoluted score depending
on just how incorrect they were.  This post is a [literate Haskell
program][gh] that starts with a few imports.

[aoc]: https://adventofcode.com/2021/day/10
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day10.lhs

> import Control.Arrow        ((***))
> import Control.Monad        (void)
> import Data.Either          (partitionEithers)
> import Data.Foldable        (find)
> import Data.List            (sort)
> import Data.List.NonEmpty   (NonEmpty((:|)))
> import Data.Void            (Void)
> import Text.Megaparsec hiding (chunk)
> import Text.Megaparsec.Char (char)

I'm reaching out to Megaparsec, renowned for its quality error
reporting.  Matching a parenthesized expression is a simple matter of
invoking `between` while not getting confused between `some` (matches
one or more) and `many` (matches zero or more).  We're never parsing
valid expressions, so the parser's return type is actually irrelevant.

> type Parser = Parsec Void String ()
> 
> inputLine,chunks,chunk :: Parser
> inputLine = void (some chunk)
> chunks = void (many chunk)
> chunk = choice (pair <$> ["()", "[]", "{}", "<>"]) where
>   pair [a,b] = void (between (char a) (char b) chunks)
> 
> parser :: String -> Either (ParseErrorBundle String Void) ()
> parser = parse inputLine "<stdin>"


Now what happens when I feed the parser with an invalid expression
from the sample input?  I can get two families of errors.

*Corrupted* lines parse as an invalid (unexpected by the parser)
character:

    <stdin>:3:13:
      |
    3 | {([(<{}[<>[]}>{[]{[(<()>
      |             ^
    unexpected '}'
    expecting '(', '<', '[', ']', or '{'

*Incomplete* lines parse as an unexpected end of input:

    <stdin>:1:25:
      |
    1 | [({(<(())[]>[[{[]{<()<>>
      |                         ^
    unexpected end of input
    expecting '(', '<', '[', '{', or '}'

Let's write a classifier using Megaparsec's error return.  It would
have the following signature:

> classify :: String -> ParseErrorBundle String Void -> Either Part1 Part2

And we'd invoke it in a general loop as such:

> main :: IO ()
> main = interact $ show . (part1 *** part2) . partitionEithers .
>                   map (\l -> either (classify l) undefined (parser l)) .
>                   lines

I use two semantic differing `Either`s here:

* The `either (classify l) undefined` is there to apply to the lines
  in error; I afford to use `undefined` where the valid lines would be
  because there shouldn't be any as per the problem statement, and I'd
  definitely want to investigate it as a bug if it were to happen.
* The `partitionEithers` downstream is there to distinguish between
  lines relevant to parts 1 and 2.

For part 1 specifically, we compute a checksum based on the first
character in error.  So let's have our classifier return it.

> type Part1 = Char

The checksum is a simple weighted sum, depending on the characters'
arbitrary values:

> part1 :: [Part1] -> Int
> part1 = sum . map syntaxScore where
>   syntaxScore ')' = 3
>   syntaxScore ']' = 57  
>   syntaxScore '}' = 1197 
>   syntaxScore '>' = 25137

Identifying the relevant lines, namely the corrupted ones, is a matter
of drilling down the `TrivialError` structure Megaparsec returns, and
checking the unexpected element is a `Tokens` and not an `EndOfInput`.

> classify _ (ParseErrorBundle (TrivialError _ (Just (Tokens (c :| []))) _ :| []) _)

In that case, the character in error is taken directy there and
returned.

>   = Left c

For part 2, we want to autocomplete a valid ending in as few
characters as possible.  So we drill down looking for an unexpected
`EndfInput` and examine its expected tokens counterpart:

> classify l (ParseErrorBundle (TrivialError _ (Just EndOfInput) ts :| []) _) =

To have the resulting string as short as possible, we extract the
closing bracket among the list.  There is necessarily one, else the
expression would actually be valid.

>   let Just (Tokens (c :| [])) = find (`elem` (Tokens . pure <$> ")]}>")) ts

We can then append it to the original string and try again.

>       l' = l ++ [c]
>   in (c :) <$> either (classify l') (const (Right [])) (parser l')

Note this time I **cannot** afford an `undefined`: at some point the
autocompleted expression is going to be valid.  I use it to return the
list terminator.

> type Part2 = [Char]

So we extracted a `Right […]` out of the incomplete expression.  Now
to munge it into the value AoC expects.  The sorting and taking the
middle value is a disguised median, though I will actually implement
it just like that anyway.

The autocomplete score, on the other hand, is a numerical base-5
decoding.  That happens not to have zeros.

> part2 :: [Part2] -> Int
> part2 = median . map (foldl (\a b -> 5*a + autocompleteScore b) 0)
>   where median xs = sort xs !! (length xs `div` 2)
>         autocompleteScore ')' = 1
>         autocompleteScore ']' = 2
>         autocompleteScore '}' = 3
>         autocompleteScore '>' = 4

This concludes today's solution.  See you next time!
