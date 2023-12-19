library(tidyverse)
library(edwards)
source("functions.R")
txt <- readLines("data/data_11.txt")

str_length(txt[1])

# double empty rows
empty_rows <- str_which(txt, "#", negate = T)
all_dots <- txt[empty_rows[1]]
parts_list <- vector("list", length(empty_rows))
curr <- 1
for (i in seq_along(empty_rows)){
  parts_list[[i]] <- c(txt[curr:empty_rows[i]], all_dots)
  curr <- empty_rows[i] + 1
}
txt2 <- c(unlist(parts_list), txt[(empty_rows[5]+1):length(txt)])
map_int(parts_list, length)
length(txt2)
str_which(txt2, "#", negate = T)

# double empty cols
mat <- str_split(txt2, "", simplify = TRUE)
dim(mat)
ff <- function(x) {
  all(x == ".")
}
empty_cols <- which(apply(mat, 2, ff))

parts_list <- vector("list", length(empty_cols))
curr <- 1
dot_mat <- mat[, 52, drop = F]
for (i in seq_along(empty_cols)){
  parts_list[[i]] <- cbind(mat[, curr:empty_cols[i]], dot_mat)
  curr <- empty_cols[i] + 1
}

mat2 <- cbind(matrix(unlist(parts_list), nrow = length(txt2)), mat[, curr:ncol(mat)])
dim(mat2)
which(apply(mat2, 2, ff))
empty_cols

# mat2 is expanded map
# Distance matrix
sum(str_count(as.vector(mat2), "#"))
ginds <- which(mat2 == "#") # galaxies
gpos <- map(ginds, index_to_pos, rows = nrow(mat2))
gmat <- matrix(unlist(gpos), ncol = 2, byrow = T)
head(gmat)
gtbl <- tibble(id = seq_along(ginds),
               row = map_int(gpos, ~pluck(., 1)),
               col = map_int(gpos, ~pluck(., 2)))
ng <- length(ginds)
head(gpos)

# distances from x to each location row in mm
dist <- function(x, mm = gmat) {
  abs(x[1] - mm[, 1]) + abs(x[2] - mm[, 2])
}
dist(gpos[[1]])
dmat <- matrix(0, nrow = ng, ncol = ng)
for (i in seq_along(ginds)){
  dmat[i, ] = dist(gpos[[i]])
}
dmat[1:6, 1:6]
sum(dmat) / 2 # answer

# Part 2 --------------

# x is text
expand_universe <- function(x, mult = 2) {
  # empty rows
  empty_rows <- str_which(x, "#", negate = T)
  all_dots <- txt[empty_rows[1]]
  parts_list <- vector("list", length(empty_rows))
  curr <- 1
  for (i in seq_along(empty_rows)){
    parts_list[[i]] <- c(txt[curr:empty_rows[i]], rep(all_dots, mult - 1))
    curr <- empty_rows[i] + 1
  }
  txt2 <- c(unlist(parts_list), txt[(empty_rows[5]+1):length(txt)])
  # empty cols
  mat <- str_split(txt2, "", simplify = TRUE)
  dim(mat)
  ff <- function(x) {
    all(x == ".")
  }
  empty_cols <- which(apply(mat, 2, ff))

  parts_list <- vector("list", length(empty_cols))
  curr <- 1
  dot_mat <- matrix(".", nrow = nrow(mat), ncol = mult - 1)
  for (i in seq_along(empty_cols)){
    parts_list[[i]] <- cbind(mat[, curr:empty_cols[i]], dot_mat)
    curr <- empty_cols[i] + 1
  }
  cbind(matrix(unlist(parts_list), nrow = length(txt2)), mat[, curr:ncol(mat)])
}

# mm is expanded map matrix
total_dist <- function(mm) {
  # Distance matrix
  sum(str_count(as.vector(mm), "#"))
  ginds <- which(mm == "#") # galaxies
  gpos <- map(ginds, index_to_pos, rows = nrow(mm))
  gmat <- matrix(unlist(gpos), ncol = 2, byrow = T)
  ng <- length(ginds)

  # distances from x to each location row in mm
  dist <- function(x, mm = gmat) {
    abs(x[1] - mm[, 1]) + abs(x[2] - mm[, 2])
  }
  dmat <- matrix(0, nrow = ng, ncol = ng)
  for (i in seq_along(ginds)){
    dmat[i, ] = dist(gpos[[i]])
  }
  sum(dmat) / 2
}

expand_universe(txt, 2) %>%
  total_dist()

dd <- map_dbl(2:6, ~total_dist(expand_universe(txt, .)))
dd
dd - lag(dd)
dd[1] + (dd[2] - dd[1]) * (1e6 - 2) # answer
