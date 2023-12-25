library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
source("code/day_17_func.R")
txt <- readLines("data/test_17.txt")
txt <- readLines("data/data_17.txt")
mm <- text_to_matrix(txt)
mm <- matrix(as.numeric(mm), nrow = nrow(mm))
mm

# Try A* algorithm ----------------
nr <- nrow(mm)
nc <- ncol(mm)
expand_grid(r = 0:2, c = 0:2) %>%
  filter(between(r, 1, nr), between(c, 1, nc), r != c)


# Run
n <- 20
mm2 <- mm[1:n, 1:n]
nr <- nrow(mm)
nc <- ncol(mm)

nrow(mm) * ncol(mm) * 4

system_time(res <- astar2(mm2))
system_time(res2 <- astar3(mm2))
system_time(res3 <- astar4(mm))

map_dbl(res2, ~.[nr, nc]) %>% min()
map_dbl(res3, ~.[nr, nc]) %>% min()
identical(res, res2)
all.equal(res2, res3)
res3[[3]][135:141, 135:141]
mm[135:141, 135:141]
res[[1]]
res <- res3
pmin(res[[1]], res[[2]], res[[3]], res[[4]])
j <- 1:6
res2[[3]][j, j]
mm[j, j]
saveRDS(res2, "out_17.RDS")
debugonce(astar3)

sum(mm[1, ], mm[, nc])
