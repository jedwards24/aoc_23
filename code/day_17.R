library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
source("code/day_17_func.R")
txt <- readLines("data/test_17b.txt")
txt <- readLines("data/test_17.txt")
txt <- readLines("data/data_17.txt")
mm <- text_to_matrix(txt)
mm <- matrix(as.numeric(mm), nrow = nrow(mm))
mm

# Try A* algorithm ----------------
nr <- nrow(mm)
nc <- ncol(mm)

# Run
n <- 20
mm2 <- mm[1:n, 1:n]
nr <- nrow(mm)
nc <- ncol(mm)

nrow(mm) * ncol(mm) * 4

# 704 solution (too high) 703 without heuristic
system_time(res <- astar2(mm))
system_time(res2 <- astar4(mm))
system_time(res3 <- brute(mm))

map_dbl(res2, ~.[nr, nc]) %>% min()
map_dbl(res3, ~.[nr, nc]) %>% min()
identical(res, res2)
all.equal(res2, res3)
res3[[2]][135:141, 135:141]
mm[135:141, 135:141]
res3[[2]][1:10, 1:10]
mm[1:10, 1:10]

