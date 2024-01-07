library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
#source("code/day_17_func.R")
txt <- readLines("data/test_21.txt")
txt <- readLines("data/data_21.txt")
mm <- text_to_matrix(txt)
garden <- mm != "#"
#mm <- matrix(as.numeric(mm), nrow = nrow(mm))
start <- which(mm == "S", arr.ind = TRUE) %>%
  as.vector()

neighbours <- function(x) {
  cbind(c(x[1] - 1, x[1] + 1, x[1], x[1]),
        c(x[2], x[2], x[2] - 1, x[2] + 1))
}

valid_move <- function(x, mat = garden) {
  x[1] >= 1 && x[1] <= nrow(mat) &&
    x[2] >= 1 && x[2] <= ncol(mat) &&
    mat[x[1], x[2]]
}

neighbours(start)
neighbours(start) %>%
  apply(1, valid_move)
mm

neighbours(start) %>%
  apply(1, neighbours, simplify = F) %>%
  do.call(rbind, .)

locs <- which(mm == "S", arr.ind = TRUE)
n_steps <- 20
for (i in 1:n_steps){
  new <- locs %>%
    apply(1, neighbours, simplify = F) %>%
    do.call(rbind, .)
  valid <- apply(new, 1, valid_move)
  locs <- new[valid, ] %>%
    as_tibble() %>%
    distinct() %>%
    as.matrix()
  cat(i)
}
locs
nrow(locs) # Answer

# visalise visited states
mm2 <- mm
for (i in 1: nrow(locs)){
  mm2[locs[i, 1], locs[i, 2]] <- "a"
}
mm2

# Part 2--------------
# Try same method as part 1 - works but won't scale well
txt
mm
garden

valid_move2 <- function(x, mat = garden) {
    mat[x[1], x[2]]
}

# modulo division but with zero replaced by divisor
mod2 <- function(x, y) {
  out <- x %% y
  out[out == 0] <- y
  out
}

mod2((0:22), 11)


locs <- which(mm == "S", arr.ind = TRUE)
n_steps <- 500
system_time(
  for (i in 1:n_steps){
    new <- locs %>%
      apply(1, neighbours, simplify = F) %>%
      do.call(rbind, .)
    rocks <- cbind(mod2(new[, 1], nrow(mm)), mod2(new[, 2], ncol(mm)))
    valid <- apply(rocks, 1, valid_move2)
    locs <- new[valid, ] %>%
      as_tibble() %>%
      distinct() %>%
      as.matrix(rownames.force = FALSE)
    if (i %% 100 == 0) cat(i, " ")
  }
)
nrow(locs)
locs
# 100 takes 5.4s, 500 takes 10 mins

# *** Solution sketch ***
#
# If the total number of steps is an odd number then a location will be reachable iff it is visited
# after an odd number of steps (similar rule with an even end).
# Work with an even number of steps, adjusting the starting locations if total steps is odd.
# I do not need to revisit previously visited states but searching through a large number of previous
# steps will be inefficient
#
# Use the repeating pattern of rocks:
# The border of both maps contains no rocks
# So I need:
# - steps to reach each loc on a single map from each border location
# - steps to each loc from the start.
# Actually just need to know what states on boundary are reached from start in an even number of steps,
# then work from each of these.
# Although, the pattern on neighbouring maps will be different so will need all border locs as starts.

