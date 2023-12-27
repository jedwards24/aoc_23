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


# Considers only legal moves but is not optimal as it does not explore all possible paths
# - the previous moves into a location matter
astar <- function(mm, start = c(1, 1)) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  hmat <- matrix(0, nr, nc)
  for (i in 1:nr){
    for(j in 1:nc){
      hmat[i, j] <- h(c(i, j), nr, nc)
    }
  }
  gmat <- matrix(Inf, nr, nc)
  gmat[1, 1] <- 0
  dmat <- matrix(0, nr, nc)
  goal <- c(nr, nc)
  olist <- list(start)
  fvec <- hmat[1, 1]
  clist <- list()
  while(length(olist) > 0){
    ind <- which.min(fvec)
    curr <- olist[[ind]]
    olist[ind] <- NULL
    f <- fvec[ind]
    fvec <- fvec[-ind]
    clist[[length(clist) + 1]] <- curr
    if (all(curr == goal)) break()
    dprev <- dmat[curr[1], curr[2]]
    for (loc in adjacent(curr[1:2], dprev)){
      if (loc[1] > nr || loc[1] < 1 || loc[2] > nc || loc[2] < 1 ||
          some(clist, ~identical(., loc))) next()
      poss_g <- gmat[curr[1], curr[2]]+ mm[loc[1], loc[2]]
      if (poss_g < gmat[loc[1], loc[2]]){
        gmat[loc[1], loc[2]] <- poss_g
        d <- direction(curr, loc)
        dmat[loc[1], loc[2]] <- if (dprev %% 10 == d) increment_dir(dprev) else d
        if (none(olist, ~identical(., loc))){
          ii <- length(olist) + 1
          olist[[ii]] <- loc
          fvec[ii] <- poss_g + hmat[loc[1], loc[2]]
        }
      }
    }
  }
  gmat
}

init_f <- function(x, hmat) {
  hmat[x[1], x[2]] + sum(mm[1:x[1], 1:x[2]]) - mm[1, 1]
}

init_g <- function(x) {
  sum(mm[1:x[1], 1:x[2]]) - mm[1, 1]
}

# matrix of states leading e & w from state c
ew <- function(x, nr, nc) {
  out <- cbind(x[1], x[2] + c(3:1, -1:-3), c(2, 2, 2, 4, 4, 4))
  out[out[, 2] > 0 & out[, 2] <= nc, , drop = FALSE]
}

# matrix of states leading n & s from state c
ns <- function(x, nr, nc) {
  out <- cbind(x[1] + c(3:1, -1:-3), x[2], c(3, 3, 3, 1, 1, 1))
  out[out[, 1] > 0 & out[, 1] <= nc, , drop = FALSE]
}

# Adjacent cells as rows of a matric
neighbours <- function(x, nr, nc) {
  switch(x[3],
         ew(x, nr, nc),
         ns(x, nr, nc),
         ew(x, nr, nc),
         ns(x, nr, nc))
}

# Expands location state to include direction of last move
astar2 <- function(mm) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  hmat <- matrix(0, nr, nc)
  for (i in 1:nr){
    for(j in 1:nc){
      hmat[i, j] <- h(c(i, j), nr, nc)
    }
  }
  goal <- c(nr, nc)
  olist <- list(c(2, 1, 3), c(3, 1, 3), c(4, 1, 3), c(1, 2, 2), c(1, 3, 2), c(1, 4, 2))
  fvec <-  map_int(olist, ~init_f(., hmat))
  gmat <- matrix(Inf, nr, nc)
  gmat[1, 1] <- 0
  glist <- list(gmat, gmat, gmat, gmat)
  for (i in seq_along(olist)){
    xx <- olist[[i]]
    glist[[xx[3]]][xx[1], xx[2]] <- init_g(xx)
  }
  clist <- list(c(1, 1, 1), c(1, 1, 4))
  while(length(olist) > 0){
    ind <- which.min(fvec)
    curr <- olist[[ind]]
    olist[ind] <- NULL
    f <- fvec[ind]
    fvec <- fvec[-ind]
    clist[[length(clist) + 1]] <- curr #
    if (all(curr[1:2] == goal)) break()
    neigh <- neighbours(curr, nr, nc)
    for (i in 1 : nrow(neigh)){
      loc <- neigh[i, ]
      if (some(clist, ~identical(., loc))) next() #
      dd <- loc[3]
      poss_g <- glist[[curr[3]]][curr[1], curr[2]] +
        sum(mm[curr[1]:loc[1], curr[2]:loc[2]]) - mm[curr[1], curr[2]]
      if (poss_g < glist[[dd]][loc[1], loc[2]]){
        glist[[dd]][loc[1], loc[2]] <- poss_g
        if (none(olist, ~identical(., loc))){ #
          ii <- length(olist) + 1
          olist[[ii]] <- loc
          fvec[ii] <- poss_g + hmat[loc[1], loc[2]]
        }
      }
    }
    if (length(clist) %% 100 == 0){
      cat("Closed:", length(clist), "open:", length(olist), "\n")
    }
  }
  glist
}

# As astar2 but uses list-matrix to record closed list
astar3 <- function(mm) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  hmat <- matrix(0, nr, nc)
  for (i in 1:nr){
    for(j in 1:nc){
      hmat[i, j] <- h(c(i, j), nr, nc)
    }
  }
  goal <- c(nr, nc)
  olist <- list(c(2, 1, 3), c(3, 1, 3), c(4, 1, 3), c(1, 2, 2), c(1, 3, 2), c(1, 4, 2))
  fvec <-  map_int(olist, ~init_f(., hmat))
  gmat <- matrix(Inf, nr, nc)
  gmat[1, 1] <- 0
  glist <- list(gmat, gmat, gmat, gmat)
  for (i in seq_along(olist)){
    xx <- olist[[i]]
    glist[[xx[3]]][xx[1], xx[2]] <- init_g(xx)
  }
  clist <- matrix(FALSE, nr, nc) %>%
    {list(., ., ., .)}
  clist[[1]][1, 1] <- clist[[4]][1, 1] <- TRUE
  while(length(olist) > 0){
    ind <- which.min(fvec)
    curr <- olist[[ind]]
    olist[ind] <- NULL
    f <- fvec[ind]
    fvec <- fvec[-ind]
    clist[[curr[3]]][curr[1], curr[2]] <- TRUE
    if (all(curr[1:2] == goal)) break()
    neigh <- neighbours(curr, nr, nc)
    for (i in 1 : nrow(neigh)){
      loc <- neigh[i, ]
      if (clist[[loc[3]]][loc[1], loc[2]]) next()
      dd <- loc[3]
      poss_g <- glist[[curr[3]]][curr[1], curr[2]] +
        sum(mm[curr[1]:loc[1], curr[2]:loc[2]]) - mm[curr[1], curr[2]]
      if (poss_g < glist[[dd]][loc[1], loc[2]]){
        glist[[dd]][loc[1], loc[2]] <- poss_g
        if (none(olist, ~identical(., loc))){ #
          ii <- length(olist) + 1
          olist[[ii]] <- loc
          fvec[ii] <- poss_g + hmat[loc[1], loc[2]]
        }
      }
    }
    tot <- sum(unlist(clist))
    if (tot %% 100 == 0){
      cat("Closed:", tot, "open:", length(olist), "\n")
    }
  }
  glist
}

# As astar3 but uses list-matrix to record open list
astar4 <- function(mm) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  hmat <- matrix(0, nr, nc)
  for (i in 1:nr){
    for(j in 1:nc){
      hmat[i, j] <- h(c(i, j), nr, nc)
    }
  }
  goal <- c(nr, nc)
  ostart <- list(c(2, 1, 3), c(3, 1, 3), c(4, 1, 3), c(1, 2, 2), c(1, 3, 2), c(1, 4, 2))
  omat <- matrix(Inf, nr, nc)
  olist <- list(omat, omat, omat, omat)
  for (x in ostart){
    olist[[x[3]]][x[1], x[2]] <- init_f(x, hmat)
  }
  gmat <- matrix(Inf, nr, nc)
  gmat[1, 1] <- 0
  glist <- list(gmat, gmat, gmat, gmat)
  for (i in seq_along(ostart)){
    xx <- ostart[[i]]
    glist[[xx[3]]][xx[1], xx[2]] <- init_g(xx)
  }
  clist <- matrix(FALSE, nr, nc) %>%
    {list(., ., ., .)}
  clist[[1]][1, 1] <- clist[[4]][1, 1] <- TRUE
  while(length(olist) > 0){
    dind <- which.min(map_dbl(olist, min))[1]
    ind <- which.min(olist[[dind]]) %>% index_to_pos(rows = nr)
    curr <- c(ind, dind)
    f <- olist[[dind]][ind[1], ind[2]]
    olist[[dind]][ind[1], ind[2]] <- Inf
    clist[[curr[3]]][curr[1], curr[2]] <- TRUE
    if (all(curr[1:2] == goal)) break()
    neigh <- neighbours(curr, nr, nc)
    for (i in 1 : nrow(neigh)){
      loc <- neigh[i, ]
      if (clist[[loc[3]]][loc[1], loc[2]]) next()
      dd <- loc[3]
      poss_g <- glist[[curr[3]]][curr[1], curr[2]] +
        sum(mm[curr[1]:loc[1], curr[2]:loc[2]]) - mm[curr[1], curr[2]]
      if (poss_g < glist[[dd]][loc[1], loc[2]]){
        glist[[dd]][loc[1], loc[2]] <- poss_g
        if (olist[[loc[3]]][loc[1], loc[2]] == Inf){
          olist[[loc[3]]][loc[1], loc[2]] <- poss_g + hmat[loc[1], loc[2]]
        }
      }
    }
    tot <- sum(unlist(clist))
    if (tot %% 100 == 0){
      cat("Closed:", tot, "open:", sum(unlist(olist) != Inf), "\n")
    }
  }
  glist
}


# As astar3 but uses list-matrix to record open list
# Zeroed heuristic
astar5 <- function(mm) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  hmat <- matrix(0, nr, nc)
  for (i in 1:nr){
    for(j in 1:nc){
      hmat[i, j] <- 0
    }
  }
  goal <- c(nr, nc)
  ostart <- list(c(2, 1, 3), c(3, 1, 3), c(4, 1, 3), c(1, 2, 2), c(1, 3, 2), c(1, 4, 2))
  omat <- matrix(Inf, nr, nc)
  olist <- list(omat, omat, omat, omat)
  ostart <- list(c(2, 1, 3), c(3, 1, 3), c(4, 1, 3), c(1, 2, 2), c(1, 3, 2), c(1, 4, 2))
  for (x in ostart){
    olist[[x[3]]][x[1], x[2]] <- init_f(x, hmat)
  }
  gmat <- matrix(Inf, nr, nc)
  gmat[1, 1] <- 0
  glist <- list(gmat, gmat, gmat, gmat)
  for (i in seq_along(ostart)){
    xx <- ostart[[i]]
    glist[[xx[3]]][xx[1], xx[2]] <- init_g(xx)
  }
  clist <- matrix(FALSE, nr, nc) %>%
    {list(., ., ., .)}
  clist[[1]][1, 1] <- clist[[4]][1, 1] <- TRUE
  while(length(olist) > 0){
    dind <- which.min(map_dbl(olist, min))[1]
    ind <- which.min(olist[[dind]]) %>% index_to_pos(rows = nr)
    curr <- c(ind, dind)
    f <- olist[[dind]][ind[1], ind[2]]
    olist[[dind]][ind[1], ind[2]] <- Inf
    clist[[curr[3]]][curr[1], curr[2]] <- TRUE
    if (all(curr[1:2] == goal)) break()
    neigh <- neighbours(curr, nr, nc)
    for (i in 1 : nrow(neigh)){
      loc <- neigh[i, ]
      if (clist[[loc[3]]][loc[1], loc[2]]) next()
      dd <- loc[3]
      poss_g <- glist[[curr[3]]][curr[1], curr[2]] +
        sum(mm[curr[1]:loc[1], curr[2]:loc[2]]) - mm[curr[1], curr[2]]
      if (poss_g < glist[[dd]][loc[1], loc[2]]){
        glist[[dd]][loc[1], loc[2]] <- poss_g
        if (olist[[loc[3]]][loc[1], loc[2]] == Inf){
          olist[[loc[3]]][loc[1], loc[2]] <- poss_g + hmat[loc[1], loc[2]]
        }
      }
    }
    tot <- sum(unlist(clist))
    if (tot %% 100 == 0){
      cat("Closed:", tot, "open:", sum(unlist(olist) != Inf), "\n")
    }
  }
  glist
}

# All possible moves
brute <- function(mm) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  hmat <- matrix(0, nr, nc)
  for (i in 1:nr){
    for(j in 1:nc){
      hmat[i, j] <- 0
    }
  }
  goal <- c(nr, nc)
  ostart <- list(c(2, 1, 3), c(3, 1, 3), c(4, 1, 3), c(1, 2, 2), c(1, 3, 2), c(1, 4, 2))
  omat <- matrix(Inf, nr, nc)
  olist <- list(omat, omat, omat, omat)
  ostart <- list(c(2, 1, 3), c(3, 1, 3), c(4, 1, 3), c(1, 2, 2), c(1, 3, 2), c(1, 4, 2))
  for (x in ostart){
    olist[[x[3]]][x[1], x[2]] <- init_f(x, hmat)
  }
  gmat <- matrix(Inf, nr, nc)
  gmat[1, 1] <- 0
  glist <- list(gmat, gmat, gmat, gmat)
  for (i in seq_along(ostart)){
    xx <- ostart[[i]]
    glist[[xx[3]]][xx[1], xx[2]] <- init_g(xx)
  }
  clist <- matrix(FALSE, nr, nc) %>%
    {list(., ., ., .)}
  clist[[1]][1, 1] <- clist[[4]][1, 1] <- TRUE
  while(TRUE){
    omins <- map_dbl(olist, min)
    if (min(omins) == Inf) break()
    dind <- which.min(map_dbl(olist, min))[1]
    ind <- which.min(olist[[dind]]) %>% index_to_pos(rows = nr)
    curr <- c(ind, dind)
    f <- olist[[dind]][ind[1], ind[2]]
    olist[[dind]][ind[1], ind[2]] <- Inf
    clist[[curr[3]]][curr[1], curr[2]] <- TRUE
#    if (all(curr[1:2] == goal)) break()
    neigh <- neighbours(curr, nr, nc)
    for (i in 1 : nrow(neigh)){
      loc <- neigh[i, ]
      if (clist[[loc[3]]][loc[1], loc[2]]) next()
      dd <- loc[3]
      poss_g <- glist[[curr[3]]][curr[1], curr[2]] +
        sum(mm[curr[1]:loc[1], curr[2]:loc[2]]) - mm[curr[1], curr[2]]
      if (poss_g < glist[[dd]][loc[1], loc[2]]){
        glist[[dd]][loc[1], loc[2]] <- poss_g
        if (olist[[loc[3]]][loc[1], loc[2]] == Inf){
          olist[[loc[3]]][loc[1], loc[2]] <- poss_g + hmat[loc[1], loc[2]]
        }
      }
    }
    tot <- sum(unlist(clist))
    if (tot %% 100 == 0){
      cat("Closed:", tot, "open:", sum(unlist(olist) != Inf), "\n")
    }
  }
  glist
}

# Without any constraints so does not solve the problem
astar_basic <- function(mm, start = c(1, 1)) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  hmat <- matrix(0, nr, nc)
  for (i in 1:nr){
    for(j in 1:nc){
      hmat[i, j] <- h(c(i, j), nr, nc)
    }
  }
  gmat <- matrix(Inf, nr, nc)
  gmat[1, 1] <- 0
  goal <- c(nr, nc)
  olist <- list(start)
  fvec <- hmat[1, 1]
  clist <- list()
  while(length(olist) > 0){
    ind <- which.min(fvec)
    curr <- olist[[ind]]
    olist[ind] <- NULL
    f <- fvec[ind]
    fvec <- fvec[-ind]
    clist[[length(clist) + 1]] <- curr
    if (all(curr == goal)) break()
    for (loc in adjacent(curr[1:2])){
      if (loc[1] > nr || loc[1] < 1 || loc[2] > nc || loc[2] < 1 ||
          some(clist, ~identical(., loc))) next()
      poss_g <- gmat[curr[1], curr[2]]+ mm[loc[1], loc[2]]
      if (poss_g < gmat[loc[1], loc[2]]){
        gmat[loc[1], loc[2]] <- poss_g
        dmat[loc[1], loc[2]] <-
        if (none(olist, ~identical(., loc))){
          ii <- length(olist) + 1
          olist[[ii]] <- loc
          fvec[ii] <- poss_g + hmat[loc[1], loc[2]]
        }
      }
    }
  }
  gmat
}
