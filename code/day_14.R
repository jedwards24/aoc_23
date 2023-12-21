# Quick to solve
library(tidyverse)
library(edwards)
source("functions.R")
txt <- readLines("data/data_14.txt")
txt <- readLines("data/test_14.txt")
mm <- text_to_matrix(txt)

sort_rocks_north <- function(x) {
  cube <- c(which(x == "#"), length(x) + 1)
  out <- x
  start <- 1
  for (i in cube){
    if (start < i){
      out[start:(i-1)] <- sort(x[start:(i-1)], decreasing = TRUE)
    }
    start <- i + 1
  }
  out
}

score <- function(x) {
  count <- apply(x, 1, function(x) sum(x == "O"))
  sum(count * rev(1:nrow(x)))
}

x <- mm[, 1]
x
sort_rocks_north(x)

new <- apply(mm, 2, sort_rocks_north)
score(new) # answer

# Part 2 ------------
sort_rocks_south <- function(x) {
  rev(sort_rocks_north(rev(x)))
}

cycle <- function(x) {
  x <- apply(x, 2, sort_rocks_north)
  x <- sort_west(x)
  x <- apply(x, 2, sort_rocks_south)
  sort_east(x)
}

sort_west <- function(x) {
  out <- x
  for (i in 1:nrow(x)){
    out[i, ] <- sort_rocks_north(x[i, ])
  }
  out
}

sort_east <- function(x) {
  out <- x
  for (i in 1:nrow(x)){
    out[i, ] <- sort_rocks_south(x[i, ])
  }
  out
}


run_cycles <- function(x, n) {
  out <- vector("list", n)
  for (i in 1:n){
    out[[i]] <- x <- cycle(x)
  }
  out
}
sort_west(mm)

bench::system_time(cycle(mm))

bench::system_time(res <- run_cycles(mm, 1009)) #2m for 1000

n_distinct(res)

# did not need this
for (i in 1:10){
  map_lgl(res, ~identical(., res[[i]])) %>% sum() %>% cat()
}

# gives cycle of 27
map_lgl(res, ~identical(., res[[1000]])) %>% which()
map_lgl(res, ~identical(., res[[1000]])) %>% which() %>% {.%%27}
1000000000 %% 27

res[[1000]] %>% score #answer

# apply not behaving as expected ------------------
apply(mm, 2, sort_rocks_north)
apply(mm, 2, sort_rocks_south)
x1 <- apply(mm, 1, sort_rocks_north) %>% matrix(nrow = nrow(mm), byrow = T)
x2 <- apply(mm, 1, sort_rocks_south) %>% matrix(nrow = nrow(mm), byrow = T)
identical(x1, sort_west(mm))
identical(x2, sort_east(mm))

f1 <- function(x){
  apply(x, 1, sort_rocks_north) %>% matrix(nrow = nrow(x), byrow = T)
}
bench::mark(f1(mm), sort_west(mm))
