# aoc19 - Advent of Code 2019

My solutions to the [Advent of code 2019](https://adventofcode.com/2019) programming challenge.

## Technologies used

- *Language:* [Haskell](https://www.haskell.org/)
- *Build Tool:* [Stack](https://docs.haskellstack.org/en/stable/README/)

## What I learned so far

- Get comfortable with `Monad`, `Functor`, `Applicative`
- Parsing Combinators with `megaparsec`


## View my solutions 

All implementations can be found in [./src/](./src).  
Generally the solution for *day x* will correspond to a `DayX.hs` source file.

If IO is required, it is handled in [./app/Main.hs](./app/Main.hs) to keep the solutions completely pure.

## How to run my solutions
```zsh
stack run -- DAY
```

e.g. for day 1

```zsh
stack run -- 1
```


