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
