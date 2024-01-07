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
for (i in 1:65){
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
nrow(locs)
