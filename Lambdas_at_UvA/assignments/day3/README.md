# Assignment for Course "Lambdas at UvA"

(C) 2016 http://softlang.org, Ralf Laemmel

## Simple assignment

See these modules:
* LinearSearch.hs
* SelectionSort.hs
* InsertionSort.hs

This assignment deals with simple use cases of foldr/l.

## Modest assignment

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

## Tough assignment

TBA
