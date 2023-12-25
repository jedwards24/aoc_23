library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
#source("code/day_16_func.R")
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

adjacent <- function(x, d) {
  out <- list(x + c(-1, 0),
              x + c(0, 1),
              x + c(1, 0),
              x + c(0, -1))
  if (d > 100) out[[round(d %% 10, 0)]] <- NULL
  out
}

direction <- function(from, to) {
  d <- to - from
  if (d[1] == -1) return(1)
  if (d[2] == 1) return(2)
  if (d[1] == 1) return(3)
  if (d[2] == -1) return(4)
}

direction <- function(from, to) {
  d <- to - from
  d[1] + 2 + (d[2] != 0) * (d[2] - 1) * -1
}

increment_dir <- function(d) {
  if (d < 10) return(d + 10 * d)
  d + d * 100
}

h <- function(x, nr, nc) {
  nr - x[1] + nc - x[2]
}

hmat <- matrix(0, nr, nc)
for (i in 1:nr){
  for(j in 1:nc){
    hmat[i, j] <- h(c(i, j), nr, nc)
  }
}

# Run
n <- 20
mm2 <- mm[1:n, 1:n]
nr <- nrow(mm2)
nc <- ncol(mm2)

nrow(mm) * ncol(mm) * 4

system_time(res <- astar3(mm2))
system_time(res2 <- astar3(mm2))

map_dbl(res, ~.[nr, nc]) %>% min()
map_dbl(res2, ~.[nr, nc]) %>% min() #61
identical(res, res2)
res <- res2
pmin(res[[1]], res[[2]], res[[3]], res[[4]])
j <- 1:6
res2[[3]][j, j]
mm[j, j]
saveRDS(res2, "out_17.RDS")
debugonce(astar3)
