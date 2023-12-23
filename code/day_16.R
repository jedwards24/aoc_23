library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
txt <- readLines("data/test_16.txt")
txt <- readLines("data/data_16.txt")
mm <- text_to_matrix(txt)

move <- function(x, mm) {
  sym <- mm[x[1], x[2]]
  i <- match(sym, c(".", "/", "\\", "|", "-"))
  out <- switch(i,
                dot2(x),
                mirrorf2(x),
                mirrorb2(x),
                pipens(x),
                pipewe(x)
  )
  out
}

# alternative
move2 <- function(x, mm) {
  sym <- mm[x[1], x[2]]
  i <- match(sym, c(".", "/", "\\", "|", "-"))
  d <- switch(i,
              x[3],
              c(2, 1, 4, 3)[x[3]],
              c(4, 3, 2, 1)[x[3]],
              pipens(x),
              pipewe(x)
  )
  c(x[1] + c(-1, 0, 1, 0)[d], x[2] + c(0, 1, 0, 1), d)
}


dot <- function(x) {
  switch(x[3],
         x + c(-1, 0, 0),
         x + c(0, 1, 0),
         x + c(1, 0, 0),
         x + c(0, -1, 0)
  )
}

mirrorb <- function(x) {
  switch(x[3],
         x + c(0, -1, 3),
         x + c(1, 0, 1),
         x + c(0, 1, -1),
         x + c(-1, 0, -3)
  )
}

mirrorf <- function(x) {
  switch(x[3],
         x + c(0, 1, 1),
         x + c(-1, 0, -1),
         x + c(0, -1, 1),
         x + c(1, 0, -1)
  )
}

pipens <- function(x) {
  if(x[3] %in% c(1, 3)) return(dot(x))
  list(c(x[1] - 1, x[2], 1), c(x[1] + 1, x[2], 3))
}

pipewe <- function(x) {
  if(x[3] %in% c(2, 4)) return(dot(x))
  list(c(x[1], x[2] + 1, 2), c(x[1], x[2] - 1, 4))
}

valid_state <- function(x, nr, nc) {
  between(x[1], 1, nr) && between(x[2], 1, nc)
}

beam_path <- function(mm, state = c(1, 1, 2), path = NULL) {
  if (is.null(path)) path <- list()
  nr <- nrow(mm)
  nc <- ncol(mm)
  if (!valid_state(state, nr, nc)) return(path)
  while (TRUE){
    path[[length(path) + 1]] <- state
    new <- move(state, mm)
    if (length(new) == 2){
      state <- new[[2]]
      path <- beam_path(mm, new[[1]], path)
    }else{
      state <- new
    }
    if (!valid_state(state, nr, nc)) return(path)
    if (length(path) %% 40 == 0){
      if (any(map_lgl(path, ~identical(., state)))){
        return(path)
      }
    }
    if (length(path) %% 1000 == 0) cat(length(path), " ")
  }
  path
}

beam_path2 <- function(mm, state = c(1, 1, 2), path = NULL) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  if (is.null(path)) path <- rep(0, nr * nc * 4)
  if (!valid_state(state, nr, nc)) return(path)
  while (TRUE){
    path[(state[3] - 1) * nr * nc + pos_to_index(state[1:2], nr)] <- 1
    new <- move(state, mm)
    if (length(new) == 2){
      state <- new[[2]]
      path <- beam_path2(mm, new[[1]], path)
    }else{
      state <- new
    }
    if (!valid_state(state, nr, nc)) return(path)
    if (path[(state[3] - 1) * nr * nc + pos_to_index(state[1:2], nr)] == 1){
      return(path)
    }
  }
  path
}

score_path <- function(pp_list) {
  x <- map_int(pp_list, ~.[1])
  y <- map_int(pp_list, ~.[2])
  out <- matrix(0, nrow = nrow(mm), ncol = ncol(mm))
  for (i in seq_along(x)){
    out[x[i], y[i]] <- 1
  }
  sum(out)
}

score_path2 <- function(x){
  matrix(x, ncol = 4) %>%
    apply(1, max) %>%
    sum()
}
system_time(pp <- beam_path(mm))
system_time(pp2 <- beam_path2(mm))

score_path(pp)
score_path2(pp2)

# Part 2 ---------------
# Get states to test
nr <- nrow(mm)
nc = ncol(mm)
tb <- expand_grid(x = 1:nrow(mm), y = 1 : ncol(mm)) %>%
  filter(x == 1 | y == 1 | x == nrow(mm) | y == ncol(mm)) %>%
  mutate(d = case_when(
    x == 1 ~ 3,
    x == nr ~ 1,
    y == 1 ~ 2,
    y == nc ~ 4))

corn <- rbind(tibble(x = c(1, 1, nr, nr), y = c(1, nc, 1, nc), d = c(2, 4, 2, 4)))

tb <- rbind(tb, corn) %>%
  rowwise() %>%
  mutate(states = list(c_across(x:d)))

sc <- numeric(nrow(tb))
tt <- numeric(nrow(tb))
for (i in seq_along(tb$states)){
  tt[i] <- system_time(sc[i] <- score_path(beam_path(mm, tb$states[[i]])))[1]
  cat("\nExper", i, "Time", tt[i], "Score", sc[i], "\n")
}
max(sc)

# second method
system_time(sc2 <- map_dbl(tb$states[1:44], ~score_path2(beam_path2(mm, .)), .progress = TRUE))

# third: check for starting states in existing paths
# **There were none so this does not help**
# 7.5 mins total
st <- tb$states
sc <- numeric(length(sc))
tt <- numeric(length(sc))
pp_done <- rep(0, nr * nc * 4)
for (i in seq_along(st)){
  state_curr <- tb$states[[i]]
  if (pp_done[(state_curr[3] - 1) * nr * nc + pos_to_index(state_curr[1:2], nr)] == 1){
    cat("\nSkip", i)
  }else{
    tt[i] <- system_time(pp_curr <- beam_path2(mm, state_curr))[1]
    pp_done <- pmax(pp_done, pp_curr)
    sc[i] <- score_path2(pp_curr)
    cat("\nExper", i, "Time", tt[i], "Score", sc[i], "\n")
  }
}
max(sc)

pp_done
sum(pp_done)
sum(tt) %>% as_bench_time()
