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

WIP