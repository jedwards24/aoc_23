# Part 2 was tricky both in working out the method and coding it
# My method was messy and took a lot of debugging to get it right
library(tidyverse)
library(edwards)
txt <- readLines("data/data_10.txt")
mat <- str_split(txt, "", simplify = TRUE)

# Row/column of position of x in matrix mat
which_mat <- function(x, mat) {
  ind <- which(x == mat)
  c((ind - 1) %% nrow(mat) + 1, ((ind - 1) %/% nrow(mat)) + 1)
}

start <- c(119, 64)
mat[118:120, 63:65] # S is a |

# | is a vertical pipe connecting north and south.
#- is a horizontal pipe connecting east and west.
#L is a 90-degree bend connecting north and east.
#J is a 90-degree bend connecting north and west.
#7 is a 90-degree bend connecting south and west.
#F is a 90-degree bend connecting south and east.
# 1N, 2E, 3S, 4W

move <- function(x, char) {
  i <- match(char, c("|", "-", "L", "J", "7", "F", "S"))
  out <- switch(i,
                pipens(x),
                pipewe(x),
                bendL(x),
                bendJ(x),
                bend7(x),
                bendF(x),
                pipens(x)
  )
  out
}

x <- c(1, 1, 1)
switch(2,
       pipens(x),
       pipewe(x))
pipens <- function(x) {
  if (x[3] == 1) return(x + c(-1, 0, 0))
  x + c(1, 0, 0)
}

pipewe <- function(x) {
  if (x[3] == 2) return(x + c(0, 1, 0))
  x + c(0, -1, 0)
}

bendL <- function(x) {
  if (x[3] == 4) return(c(x[1] - 1, x[2], 1)) #west
  c(x[1], x[2] + 1, 2)
}

bendJ <- function(x) {
  if (x[3] == 2) return(c(x[1] - 1, x[2], 1)) #east
  c(x[1], x[2] - 1, 4)
}

bend7 <- function(x) {
  if (x[3] == 2) return(c(x[1] + 1, x[2], 3)) #east
  c(x[1], x[2] - 1, 4) #N
}

bendF <- function(x) {
  if (x[3] == 4) return(c(x[1] + 1, x[2], 3)) #west
  c(x[1], x[2] + 1, 2)
}

# Does not store start/finish
start <- c(119, 64)
pos <- c(start, 1)
moves <- list()
for (i in seq_along(mat)){
  char <- mat[pos[1], pos[2]]
  pos <- move(pos, char)
  if (all(pos[1:2] == start)) break()
  moves[[i]] <- pos
}
moves
length(moves)
pos
moves[[1]]
pmin(1:length(moves), length(moves):1) %>% max #answer

# part 2-------------
# Run part 1 first to get path

# vector index of row/col position vector in matrix
pos_to_index <- function(x, rows = 140) {
  x[1] + ((x[2] - 1) * rows)
}

#inverse of pos_to_index
index_to_pos <- function(x, rows = 140) {
  c((x - 1) %% rows + 1, ((x - 1) %/% rows) + 1)
}

sum(mat == ".")
dot_inds <- which(mat == ".")
pipe_inds <- map_int(moves, pos_to_index) %>%
  c(pos_to_index(start))
140^2 - length(pipe_inds) #6124
mat2 <- mat
mat2[-pipe_inds] <- "."
txt2 <- apply(mat2, 1, paste0, collapse = "")
writeLines(txt2, "out_10.txt")

# Classify outers where obvious
check_outer_simple <- function(ind, mm = mat2) {
  loc <- index_to_pos(ind)
  all(mm[1:loc[1], loc[2]] == ".") |
    all(mm[loc[1]:nrow(mm), loc[2]] == ".") |
    all(mm[loc[1], 1:loc[2]] == ".") |
    all(mm[loc[1], loc[2]:ncol(mm)] == ".")
}
sym_vec <- as.vector(mat2)
outer_lgl <- rep(FALSE, length(mat))
for (i in seq_along(outer_lgl)){
  if (sym_vec[i] == "."){
    outer_lgl[i] <- check_outer_simple(i)
  }
}
outer_simp_inds <- which(outer_lgl)
length(outer_simp_inds) #5235

dot_inds2 <- which(sym_vec == ".") # all dots in mat2
dots_to_check <- setdiff(dot_inds, outer_simp_inds)
dots_to_check2 <- setdiff(dot_inds2, outer_simp_inds)

length(dots_to_check) #92
length(dots_to_check2) #889

# next:
# New move function which follows pipe but tracks which side of it you are on.
# At each step check if adjacent square in direction away from pipe is in outer_simp_inds.
# If so it is outer. Check full length of pipe.
# Could group dots into contiguous blocks and only check one square from each.

# Helper for direction modulo arithmetic
news <- function(x) {
  x <- x %% 4
  if (x == 0) x <- 4
  x
}

move <- function(x, char) {
  if (char == ".") return(c(x[1] - 1, x[2:4]))
  i <- match(char, c("|", "-", "L", "J", "7", "F", "S"))
  out <- switch(i,
                pipens(x),
                pipewe(x),
                bendL(x),
                bendJ(x),
                bend7(x),
                bendF(x),
                pipens(x)
  )
  out
}

pipens <- function(x) {
  if (x[3] == 1) return(x + c(-1, 0, 0, 0))
  x + c(1, 0, 0, 0)
}

pipewe <- function(x) {
  if (x[3] == 1) return(c(x[1], x[2] + 1, 2, 3))
  if (x[3] == 2) return(x + c(0, 1, 0, 0))
  x + c(0, -1, 0, 0)
}

bendL <- function(x) {
  if (x[3] == 1) return(c(x[1] - 1, x[2], 1, 4))
  if (x[3] == 4) return(c(x[1] - 1, x[2], 1, x[4] + 1)) #west
  c(x[1], x[2] + 1, 2, x[4] - 1)
}

bendJ <- function(x) {
  if (x[3] == 1) return(c(x[1] - 1, x[2], 1, 2))
  if (x[3] == 2) return(c(x[1] - 1, x[2], 1, news(x[4] - 1))) #east
  c(x[1], x[2] - 1, 4, news(x[4] + 1))
}

bend7 <- function(x) {
  if (x[3] == 2) return(c(x[1] + 1, x[2], 3, x[4] + 1)) #east
  c(x[1], x[2] - 1, 4, x[4] - 1) #N
}

bendF <- function(x) {
  if (x[3] == 4) return(c(x[1] + 1, x[2], 3, news(x[4] - 1))) #west
  c(x[1], x[2] + 1, 2, news(x[4] + 1))
}

# TRUE if adjacent square on current side is an outer square
# x is state
check_adj_outer <- function(x, outer = outer_simp_inds) {
  loc <- switch(x[4],
                c(x[1] - 1, x[2]),
                c(x[1], x[2] + 1),
                c(x[1] + 1, x[2]),
                c(x[1], x[2] - 1))
  return(pos_to_index(loc) %in% outer)
}

# See if there is a way out starting in x
# x is index
test_escape <- function(x, mm = mat2) {
  pos <- c(index_to_pos(x), 1, 3)
  char <- mm[pos[1], pos[2]]
  for (i in 1 : (length(moves) + 140)){
    if (check_adj_outer(pos)) return(TRUE)
    char <- mm[pos[1], pos[2]]
    pos <- move(pos, char)
  }
  FALSE
}

outer_lgl <- map_lgl(dots_to_check2, test_escape, .progress = TRUE)

length(dots_to_check2)
sum(!outer_lgl) # answer
