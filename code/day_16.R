library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
source("code/day_16_func.R")
txt <- readLines("data/test_16.txt")
txt <- readLines("data/data_16.txt")
mm <- text_to_matrix(txt)

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

# first method
st <- tb$states
sc <- numeric(length(st))
tt <- numeric(length(st))
for (i in seq_along(st)){
  tt[i] <- system_time(sc[i] <- score_path(beam_path(mm, tb$states[[i]])))[1]
  cat("\nExper", i, "Time", tt[i], "Score", sc[i], "\n")
}
max(sc)
sum(tt) %>% as_bench_time()

# second, third, and fourth methods
system_time(sc2 <- map_dbl(tb$states[1:44], ~score_path2(beam_path2(mm, .)), .progress = TRUE))
system_time(sc3 <- map_dbl(tb$states[1:44], ~score_path2(beam_path3(mm, .)), .progress = TRUE))
system_time(sc4 <- map_dbl(tb$states[1:44], ~score_path2(beam_path4(mm, .)), .progress = TRUE))

# fifth: using beam_path2 but check for starting states in existing paths
# **There were none so this does not help**
# 7.5 mins total
st <- tb$states
sc <- numeric(length(st))
tt <- numeric(length(st))
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
