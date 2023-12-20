# vector index of row/col position vector in matrix
pos_to_index <- function(x, rows = 140) {
  x[1] + ((x[2] - 1) * rows)
}

#inverse of pos_to_index
index_to_pos <- function(x, rows = 140) {
  c((x - 1) %% rows + 1, ((x - 1) %/% rows) + 1)
}

# Row/column of position of x in matrix mat
which_mat <- function(x, mat) {
  ind <- which(x == mat)
  c((ind - 1) %% nrow(mat) + 1, ((ind - 1) %/% nrow(mat)) + 1)
}

# Convert x to binary vector of length `bits`
int2bin = function(x, bits) {
  rev(as.numeric(intToBits(x))[1:bits])
}
