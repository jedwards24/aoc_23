# Quick Notes

# Notes

`pluck()` is nicer to work with for assigning and retrieving items from lists. Avoids `x[[1]][[2]]` type code.

# Things to look up

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





