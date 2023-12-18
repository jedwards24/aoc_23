library(tidyverse)
library(edwards)
txt <- readLines("data/data_9.txt")
lst <- str_split(txt, " ") %>%
  map(as.numeric)
lst[[1]]

# x is numeric vector
diff <- function(x) {
  (x - lag(x))[-1]
}

# x is numeric vector
predict_next <- function(x) {
  slist <- list(x)
  curr <- x
  for (i in seq_along(x)){
    curr <- diff(curr)
    slist[[i+1]] <- curr
    if (length(unique(curr)) == 1) break()
  }
  fval <- 0
  for (i in length(slist):1){
    xi <- slist[[i]]
    fval <- fval + xi[length(xi)]
  }
  fval
}

x <- seq(3, 15, by = 3)
x <- c(1,   3,   6,  10,  15, 21)
predict_next(x)
preds <- map_dbl(lst, predict_next)
sum(preds) # answer
x <- lst[[35]]
x

# part 2--------------
predict_next2 <- function(x) {
  slist <- list(x)
  curr <- x
  for (i in seq_along(x)){
    curr <- diff(curr)
    slist[[i+1]] <- curr
    if (length(unique(curr)) == 1) break()
  }
  fval <- 0
  for (i in length(slist):1){
    xi <- slist[[i]]
    fval <- xi[1] - fval
  }
  fval
}

x <- c(1,   3,   6,  10,  15, 21)
predict_next2(x)
preds2 <- map_dbl(lst, predict_next2)
sum(preds2) # answer
