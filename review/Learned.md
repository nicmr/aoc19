# Review


Just a collection of
 - what I learned solving the exercises
 - things I believe I should keep in mind when I write more Haskell in the future
 - other random thoughts :)




## Day 1

This one was rather easy, not much to say. Recursion and accumulators are as sexy as ever.


## Day 2

#### Exposing to ghci
Exposing everything with `module Name where` and omitting the braces does wonders for testing stuff out on ghci.  
Which I should definitely do **before** composing all the functions together. It makes mistakes way easier to spot early on.


#### Less tuples, cool list patterns
I also keep it in mind not to overuse tuples, they are way less flexible compared to Lists.
Lists with a specific number of elements (e.g. , four) can be matched with the pattern
```Haskell
(a:(b:(c:(d:xs))))
```
which looks a bit ugly but can be super useful and often avoids using a tuple.

#### Maybe ♥ do
I also shouldn't be afraid of using do monads when chaining `Maybe` and `fmap` is not appropriate.


#### Extract operations from complex recursive operations to allow for `stepping through` with ghci

When writing recursive functions that deal with a lot of state changes in its operands,
it can be beneficial to write simple `State -> State` functions (*endomorphisms*, iirc) that allow for manually performing the same operations from ghci for debugging etc.
This function can usually be used without an issue to perform the state transformations in the recursive function, before it recursively calls itself.

If there's only the recursive function, it's really hard to see what's going on (and possibly, going wrong) inside from ghci.


## Day 3

I took way too long for this one and I made tons of mistakes.
The biggest problem was probably not knowing list comprehensions well enough:
```
-- the bug
[x | x <- [20..10]]

-- result: []

-- correct approach

[x | x <-  [20,19..10]]

-- result: [1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]
```
As it doesn't crash and just return empty lists, it silently messed up all the data strucutres relying on list comprehensions. Ouchies.

Something I believe I've been doing wrong all along is writing a "pure main" that composes all other pure functions, and calling only that "pure main" from my actual `IO` main.
That sucks bigtime when composing everything in the pure main and only returning insufficient information (worst case: `Nothing`), effectively losing all the information where something went wrong along the way.

`tasty-hunit` for testing is great!!! It reminds me a lot of elm-test. I'd also like to try out `quickcheck` soon.
Maybe TDD-ish approaches will be effective for the next exercises.

My program also took almost 18 minutes to run and allocated about 400 mb. I'm not sure how I can reach a more efficient implementation with Haskell (I'd certainly know how to approach it in Rust).


## Day4

The first task was an absolute breeze, completed it in 15 minutes.
List comprehensions in haskell are perfect for this.
The performance is also surprisingly good, considering it's not a mathematical approach but rather trial-and-filter.
I'm pretty sure it's possible to write a proper mathematical alogorithm and wrote down some basic ideas, but I don't have the time today to work that out.

For the second task, I solved the doubles-but-not-triples part of the exercise using `Data.Array`. The syntax seemed a bit verbose at first compared to imperative languages but overall it made a lot of sense to me.
Great fun.



## Day6

I got stuck too long trying to solve the problem with trees. Unfortunately, due to the unsorted nature of the input, building the tree requires a lot of modifying stuff in place - which is very annoying in Haskell.
I eventually opted for not using trees at all and implemented it with a very neat hashmap solution.

For task 2, I first had the most ugly Either / Maybe case handling you could imagine.
I wasn't sure how to use a do monad when the types of the Either / Maybe function calls do not match the type parameteres of the surrounding function. I tried solving it with fmap but that only helps as long as you need to work with one Functor at a time... Fortunately, I eventually discovered `Bifunctor` and wrote a short function for escalating Maybes into eithers. I'm quite happy with the result:

```haskell
task2' :: String -> Either String String
task2' input = do
    orbits <- first (show) $ parse parseOrbits "" input 
    -- create hashmap from orbits
    let orbitmap = foldl' (\hashmap (center, orbit) -> Map.alter (appendOrCreate orbit) center hashmap) Map.empty orbits
    -- calculate paths to root
    pathToSanta <- toEither "failure at Path to Santa" $ path "COM" "SAN" [] orbitmap
    pathToYou <- toEither "failure at Path to You" $ path "COM" "YOU" [] orbitmap
    -- find index of split node
    (indexSan, indexYou) <- toEither "unable to find shared member" $ firstSharedMemberIndices pathToSanta pathToYou
    pure $ foldl' (++) "" [show pathToSanta, "and ", show pathToYou, "\nTransfers required:", show (indexSan + indexYou)] 
```