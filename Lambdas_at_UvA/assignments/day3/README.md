# Assignment for Course "Lambdas at UvA"

(C) 2016 http://softlang.org, Ralf Laemmel

## Simple assignment

See these modules:
* LinearSearch.hs
* SelectionSort.hs
* InsertionSort.hs

This assignment deals with simple use cases of foldl/r.

## Modest assignment

Consider this definition of binary search:

<pre>
-- Polymorphic binary search
-- Assume that input list is sorted
search :: Ord a => [a] -> a -> Bool
search [] _ = False
search xs x =
   if x < y then search ys1 x
   else if x > y then search ys2 x
   else True
  where
    ys1 = take l xs
    (y:ys2) = drop l xs
    l = length xs `div` 2
</pre>

This formulation is good for basic understanding, but its performance is bad.

Transform the definition into one that does not use "take" and "drop"; instead it uses "!!" for indexing.

The resulting reformulation will still be inefficient, if we assume that "!!" has linear complexity.

See the tough assigmnent option 1.

## Tough assignment

### Tough assignment option 1

Use Data.Array instead of lists in the implementation of binary search.

Use the solution for modest assignment as the starting point.

Available documentation:
https://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array.html

You should have Data.Array readily installed.

### Tough assignment option 2

See the module Term.hs.

This assignment deals with type classes in a not so trivial manner.

It also exercises Data.Map.

Available documentation:
https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Strict.html

You should have Data.Map readily installed.

### Tough assignment option 3

See the module Filter.hs.

This assignment is about coding the Prelude's filter function in a certain style.

This requires very little code, but a smart idea.
