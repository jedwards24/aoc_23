
# Lookups----------
move <- list("R" = c(0, 1), "L" = c(0, -1), "U" = c(-1, 0), "D" = c(1, 0))
lu <- c("R", "D", "L", "U") # used in initial parsing in part two

# Corner adjustment
# 1 TL, 2 TR, 3 BR, 4 BL
adj_x <- c(-0.5, 0.5, 0.5, -0.5)
adj_y <- c(-0.5, -0.5, 0.5, 0.5)
corners <- list(RU = c(1, 1, 3, 3),
                RD = c(2, 2, 4, 4),
                UL = c(4, 2, 2, 4),
                UR = c(1, 3, 3, 1),
                LU = c(2, 2, 4, 4),
                LD = c(1, 1, 3, 3),
                DL = c(1, 3, 3, 1),
                DR = c(4, 2, 2, 4))


#Functions-------

# Used to estimate size of matrix needed
cum_tbl <- function(tb) {
  tb %>%
    mutate(vert = case_when(
      dir == "U" ~ -len,
      dir == "D" ~ len,
      TRUE ~ 0
    )) %>%
    mutate(horiz = case_when(
      dir == "R" ~ len,
      dir == "L" ~ -len,
      TRUE ~ 0
    )) %>%
    mutate(cv = cumsum(vert),
           ch = cumsum(horiz))
}

# This just returns the location of the dig corners
dig_verts <- function(tb, corner_start = 1) {
  d_vec <- tb$dir
  len_vec <- tb$len
  curr <- c(1, 1)
  verts <- matrix(NA, length(d_vec), 2)
  corns <- integer(length(d_vec))
  cori <- corner_start
  for (i in seq_along(d_vec)){
    verts[i, ] <- curr
    corns[i] <- cori
    curr <- curr + len_vec[i] * move[[d_vec[i]]]
    if (i < length(d_vec)){
      cori <- corners[[str_flatten(d_vec[i:(i+1)])]][cori]
    }
  }
  verts <- rbind(verts, verts[1, ])
  corns <- c(corns, corns[1])
  cbind(verts[, 1] + adj_y[corns], verts[, 2] + adj_x[corns])
}

# The matrix I used has space around the path on all sides so I can do the fill from the top corner
# but this will not work generally
flood_fill <- function(mm) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  todo <- matrix(FALSE, nr, nc)
  todo[1, 1] <- TRUE
  while (sum(todo) > 0){
    cur <- which(todo, arr.ind = TRUE)[1, ]
    y <- cur[1]
    x <- cur[2]
    todo[y, x] <- FALSE
    hh <- c(0, which(mm[y, ] == "#"), nc + 1)
    fill <- (max(hh[hh < x]) + 1) : (min(hh[hh > x]) - 1)
    mm[y, fill] <- "*"
    todo[y, fill] <- FALSE
    if (y < nr){
      next_row <- intersect(which(mm[y + 1, ] == "."), fill)
      todo[y + 1, next_row] <- TRUE
    }
    if (y > 1){
      prev_row <- intersect(which(mm[y - 1, ] == "."), fill)
      todo[y - 1, prev_row] <- TRUE
    }
  }
  mm
}
