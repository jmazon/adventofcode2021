---
title: "AoC Day 24: Arithmetic Logic Unit"
author: Jean-Baptiste Mazon
date: 2021-12-24T18:59:23+01:00
tags: [ "advent of code", aoc2021, haskell ]
description: My tradeoff between full paper&pencil and SMT
image: aoc-haskell.jpeg
flags: [ mathjax ]
---

<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" type="text/javascript" async></script>

At last!  For Advent of Code's penultimate puzzle, day 24 [“Arithmetic
Logic Unit”][aoc] gives us a bit of assembly to analyze!  This one
was all the more interesting to me that my approach fell somewhere in
the middle of the spectrum, between the “throw Z3 at it” and the
“full reverse engineering” crowds.  This post is still [literate
Haskell][gh], headered with a few imports.

[aoc]: https://adventofcode.com/2021/day/24
[gh]: https://github.com/jmazon/adventofcode2021/blob/master/day24.lhs

> import Control.Applicative ((<|>))
> import Control.Arrow       ((&&&))
> import Control.Monad       (guard)
> import Data.Ix             (inRange)
> import Data.List           (unfoldr)
> import qualified Data.IntMap.Strict as Map

There is some structure to the input.  Namely, it can be split in
14 parts, differing only in three constants.  My parser code will make
this clearer:

> parse :: String -> ([Int],[Int],[Int])
> parse = unzip3 . unfoldr block . lines where
>   block ls = do
>     "inp w"
>       : "mul x 0"
>       : "add x z"
>       : "mod x 26"
>       : (words -> ["div","z",read -> d])
>       : (words -> ["add","x",read -> δx])
>       : "eql x w"
>       : "eql x 0"
>       : "mul y 0"
>       : "add y 25"
>       : "mul y x"
>       : "add y 1"
>       : "mul z y"
>       : "mul y 0"
>       : "add y w"
>       : (words -> ["add","y",read -> δy])
>       : "mul y x"
>       : "add z y"
>       : ls' <- pure ls
>     pure ((δx,δy,d),ls')

But what does it do?  Let's rewrite it to pseudocode in [SSA
form][ssa] or clarity.

[ssa]: https://en.wikipedia.org/wiki/Static_single_assignment_form

```
w_1 ← {input}
x_1 ← x_0 × 0
x_2 ← x_1 + z_0
x_3 ← x_2 % 26
z_1 ← z_0 / d
x_4 ← x_3 + δx
x_5 ← x_4 = w_1
x_6 ← x_5 = 0
y_1 ← y_0 × 0
y_2 ← y_1 + 25
y_3 ← y_2 × x_6
y_4 ← y_3 + 1
z_2 ← z_1 × y_4
y_5 ← y_4 × 0
y_6 ← y_5 + w_1
y_7 ← y_6 + δy
y_8 ← y_7 × x_6
z_3 ← z_2 + y_8
```

Replacing downwards and pruning temporaries, we get:

```
w_1 ← {input}
x_6 ← ((x_0 × 0 + z_0) % 26 + δx = {input}) = 0
y_8 ← (((y_0 × 0 + 25) × (((x_0 × 0 + z_0) % 26 + δx = {input}) = 0) + 1) × 0 + w_1 + δy) × (((x_0 × 0 + z_0) % 26 + δx = {input}) = 0)
z_3 ← z_0 / d × ((y_0 × 0 + 25) × (((x_0 × 0 + z_0) % 26 + δx = {input}) = 0) + 1) + y_8
```

Those two last expressions are a litle scary.  But we've got a lot of
`×0` hanging around; let's simplify those and remove a few more
parentheses.

```
w_1 ← {input}
x_6 ← (z_0 % 26 + δx = {input}) = 0
y_8 ← ({input} + δy) × ((z_0 % 26 + δx = {input}) = 0)
z_3 ← z_0 / d × (25 × ((z_0 % 26 + δx = {input}) = 0) + 1)
      + (w_1 + δy) × ((z_0 % 26 + δx = {input}) = 0)
```

What's really notable at this point is none of the `w_0`, `x_0` and
`y_0` variables are ever referenced.  So the only register that is
effectively transmitted from a block to the next is Z.  Let's make its
transfer function explicit.

$$
f_i(z) = (z\div d_i) (25x + 1) + (\mathrm{in}_i + δy_i) x \\
\quad\text{where}\quad \\
x = z\%26 + δx_i \neq \mathrm{in}_i
$$

Extracting and simplifying two actual cases from the X variable:

$$
f_i(z) =
\begin{cases}
z\div d_i & \text{if}\quad \mathrm{in}_i = z\%26 + δx_i \\
26(z\div d_i) + \mathrm{in}_i + δy_i & \text{otherwise}
\end{cases}
$$

It was possible to go even further than this, and analyze the
relationships between the $δx_i$, $δy_i$ and $d_i$ parameters.  I did
without, as that function already seems reversible enough.

How do we reverse a case-based function such as this one?  We reverse
each branch one by one, and verify the precondition afterwards.

Let's formalize a bit before we get lost.  We know the parameters.  We
know the ALU's starting state, though only its Z register matters: it
starts at 0.  We are looking for the lexicographically maximal
$\{\mathrm{in}_i\}$ values such that the resulting Z register ends at 0
after applying all transfer functions.

It's a bit much to do it all at once.  But by relaxing the intermediate
stages to “and let's see what Z prior values we get out of this”, the
search space is surprisingly^[Rumor has it it's not so much of a
surprise anymore if you actually analyze the parameters.] small.

So here's the resulting function.  It takes the usual $δx_i$, $δy_i$
and $d_i$ parameters, and the $(z',q)$ pair of Z value we're
reversing for decorated with the input stream it corresponds to.  It
returns a list, possibly empty, of $(z_0,q')$ pairs of initial Z
values and *full* corresponding input stream.  I'm keeping the input
streams in full instead of operating digit by digit so that I can
compare them directly, as part 1 asks for the maximal possible digit
sequence.

> revStage :: Int -> Int -> Int -> (Int,[Int]) -> [(Int,[Int])]
> revStage δx δy d (z',is) =
>     do
>       z0 <- (z'*d +) <$> [0..d-1] -- z' = z0 `div` d
>       let i = z0 `mod` 26 + δx
>       guard (inRange (1,9) i)
>       pure (z0,i:is)
>   <|>
>     do
>       i <- [1..9]
>       let (q,r) = (z' - i - δy) `divMod` 26
>       guard (r == 0)
>       z0 <- [q,q+1..q+d-1]
>       guard (i /= z0 `mod` 26 + δx)
>       pure (z0,i:is)

Note that while the forward version of $f_i$ can only fall in a single
case, the reverse could very well yield no results at all, or multiple
per branch!

Solving the problem is now a simple matter of chaining the reverse
stages together, and seeing which input streams we have for a starting
Z of 0.  I'll take a selector operator as an argument to ease part 2.

> solve :: ([Int] -> [Int] -> [Int]) -> ([Int],[Int],[Int]) -> Maybe [Int]
> solve op (δxs,δys,ds) =
>     Map.lookup 0 $ foldr rs (Map.singleton 0 []) (zip3 δxs δys ds)
>   where
>     rs (δx,δy,d) zqs =
>       Map.fromListWith op $ concatMap (revStage δx δy d) $ Map.assocs zqs

A small wrapper solves both variants.^[In sequence.  There would be a
nice way to do both at once with monoids and all, but come on, it
just takes one second, it's fine as is.]

> main :: IO ()
> main = interact $ show . (solve max &&& solve min) . parse

This concludes today's solution.  See you tomorrow!
