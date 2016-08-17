# Assignment for Course "Lambdas at UvA"

(C) 2016 http://softlang.org, Ralf Laemmel

## Simple assignment

See these modules:
* LinearSearch.hs
* SelectionSort.hs
* InsertionSort.hs

This assignment deals with simple use cases of foldr/l.

## Modest assignment

### Modest assignment option 1

Consider this definition of filter:

<pre>
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) =
  if p x
    then x : filter p xs
    else filter p xs
</pre>

Transform the definition into one with only one recursive occurrence of filter.

Do not use any local scope ("where").

Do not cause any performance penalty (such as with "concatenation").

### Modest assignment option 2

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

The resulting reformulation will still be inefficient, if assume that "!!" also implies linear complexity.

See the tough assigmnent option 1.

## Tough assignment

### Tough assignment option 1

Use Data.Array instead of lists in a more efficient implementation of binary search, thereby continuing modest assignment option 2.

https://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array.html

You should have Data.Array readily installed.

### Tough assignment option 2

TBA
