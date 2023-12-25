# Quick Notes

# Notes

`pluck()` is nicer to work with for assigning and retrieving items from lists. Avoids `x[[1]][[2]]` type code.

# Notes on Problems

## Day 16 speed progression

1. Checking a list of visited states for a match to the current state was a bottleneck as the list grew. Replacing this with a binary vector for all states brought a major improvement (~x6 faster).
2. Using a switch statement within the symbol functions to get the new states was maybe ~20% faster.
3. `beam_path3()` removed the symbol functions which shortened the code a lot but did not change the speed. 
4. `beam_path4()` replaced recursion with a todo stack. Maybe slightly (2%) faster.
5. I checked to see if starting states had been visited by previous solutions, in which case there was no need to do them, but there were no overlaps so this did not help. Thinking later this will always be the case.

5.5 mins for the full problem. I've seen python implementations take a few seconds (partly due to a cleverer implementation using complex numbers for the moves).

# Things to look up

## Working with lits

Remove elements from a list with `list[1] = NULL` or `list[[1]] = NULL` (seem to be the same).

Appending vector `x` to list `y` (from d16):

* `append(x, y)` and `c(y, x)` flatten the vector you are adding. 
* `list(x, y)` isn't right. 
* `y[[length(y) + 1]] <- x` is correct

```
y <- list(1, 2)
x <- 1:3
append(x, y)
c(y, x)
list(y, x)
y[[3]] <- x 
```






