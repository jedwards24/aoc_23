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

# vector index of row/col position vector in matrix
pos_to_index <- function(x, rows = 140) {
  x[1] + ((x[2] - 1) * rows)
}

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
check_outer_simple <- function(ind, mat = mat2) {
  loc <- index_to_pos(ind)
  all(mat[1:loc[1], loc[2]] == ".") |
    all(mat[loc[1]:nrow(mat), loc[2]] == ".") |
    all(mat[loc[1], 1:loc[2]] == ".") |
    all(mat[loc[1], loc[2]:ncol(mat)] == ".")
}
vv <- as.vector(mat2)
outer_lgl <- rep(FALSE, length(mat))
for (i in seq_along(outer_lgl)){
  if (vv[i] == "."){
    outer_lgl[i] <- check_outer_simple(i)
  }
}
outer_simp_inds <- which(outer_lgl)
length(outer_simp_inds) #5235

dots_to_check <- setdiff(dot_inds, outer_simp_inds)
length(dots_to_check) #92

# next:
# New move function which follows pipe but tracks which side of it you are on.
# At each step check if adjacent square in direction away from pipe is in outer_simp_inds.
# If so it is outer. Check full length of pipe.
