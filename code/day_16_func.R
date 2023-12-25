
move <- function(x, mm) {
  sym <- mm[x[1], x[2]]
  i <- match(sym, c(".", "/", "\\", "|", "-"))
  out <- switch(i,
                dot(x),
                mirrorf(x),
                mirrorb(x),
                pipens(x),
                pipewe(x)
  )
  out
}

# Alternative which gives much shorter code may be slower (or similar)
# Used in beam_path3
move2 <- function(x, mm) {
  sym <- mm[x[1], x[2]]
  i <- match(sym, c(".", "/", "\\", "|", "-"))
  d <- switch(i,
              x[3],
              c(2, 1, 4, 3)[x[3]],
              c(4, 3, 2, 1)[x[3]],
              list(1, c(1, 3), 3, c(1, 3))[[x[3]]],
              list(c(2, 4), 2, c(2, 4), 4)[[x[3]]]
  )
  c(x[1] + c(-1, 0, 1, 0)[d], x[2] + c(0, 1, 0, -1)[d], d)
}


dot <- function(x) {
  switch(x[3],
         x + c(-1, 0, 0),
         x + c(0, 1, 0),
         x + c(1, 0, 0),
         x + c(0, -1, 0)
  )
}

mirrorb <- function(x) {
  switch(x[3],
         x + c(0, -1, 3),
         x + c(1, 0, 1),
         x + c(0, 1, -1),
         x + c(-1, 0, -3)
  )
}

mirrorf <- function(x) {
  switch(x[3],
         x + c(0, 1, 1),
         x + c(-1, 0, -1),
         x + c(0, -1, 1),
         x + c(1, 0, -1)
  )
}

pipens <- function(x) {
  if(x[3] %in% c(1, 3)) return(dot(x))
  list(c(x[1] - 1, x[2], 1), c(x[1] + 1, x[2], 3))
}

pipewe <- function(x) {
  if(x[3] %in% c(2, 4)) return(dot(x))
  list(c(x[1], x[2] + 1, 2), c(x[1], x[2] - 1, 4))
}

valid_state <- function(x, nr, nc) {
  between(x[1], 1, nr) && between(x[2], 1, nc)
}

beam_path <- function(mm, state = c(1, 1, 2), path = NULL) {
  if (is.null(path)) path <- list()
  nr <- nrow(mm)
  nc <- ncol(mm)
  if (!valid_state(state, nr, nc)) return(path)
  while (TRUE){
    path[[length(path) + 1]] <- state
    new <- move(state, mm)
    if (length(new) == 2){
      state <- new[[2]]
      path <- beam_path(mm, new[[1]], path)
    }else{
      state <- new
    }
    if (!valid_state(state, nr, nc)) return(path)
    if (length(path) %% 40 == 0){
      if (any(map_lgl(path, ~identical(., state)))){
        return(path)
      }
    }
    if (length(path) %% 1000 == 0) cat(length(path), " ")
  }
  path
}

beam_path2 <- function(mm, state = c(1, 1, 2), path = NULL) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  if (is.null(path)) path <- rep(0, nr * nc * 4)
  if (!valid_state(state, nr, nc)) return(path)
  while (TRUE){
    path[(state[3] - 1) * nr * nc + pos_to_index(state[1:2], nr)] <- 1
    new <- move(state, mm)
    if (length(new) == 2){
      state <- new[[2]]
      path <- beam_path2(mm, new[[1]], path)
    }else{
      state <- new
    }
    if (!valid_state(state, nr, nc)) return(path)
    if (path[(state[3] - 1) * nr * nc + pos_to_index(state[1:2], nr)] == 1){
      return(path)
    }
  }
  path
}


beam_path3 <- function(mm, state = c(1, 1, 2), path = NULL) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  if (is.null(path)) path <- rep(0, nr * nc * 4)
  if (!valid_state(state, nr, nc)) return(path)
  while (TRUE){
    path[(state[3] - 1) * nr * nc + pos_to_index(state[1:2], nr)] <- 1
    new <- move2(state, mm)
    if (length(new) == 3){
      state <- new
    }else{
      state <- new[c(2, 4, 6)]
      path <- beam_path3(mm, new[c(1, 3, 5)], path)
    }
    if (!valid_state(state, nr, nc)) return(path)
    if (path[(state[3] - 1) * nr * nc + pos_to_index(state[1:2], nr)] == 1){
      return(path)
    }
  }
  path
}

# No recursion
beam_path4 <- function(mm, start = c(1, 1, 2), path = NULL) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  if (is.null(path)) path <- rep(0, nr * nc * 4)
  todo <- list(start)
  while (length(todo) > 0){
    state <- todo[[1]]
    todo[1] <- NULL
    if (!valid_state(state, nr, nc)) next()
    while(TRUE){
      path[(state[3] - 1) * nr * nc + pos_to_index(state[1:2], nr)] <- 1
      new <- move(state, mm)
      if (length(new) == 2){
        state <- new[[2]]
        todo[[length(todo) + 1]] <- new[[1]]
      }else{
        state <- new
      }
      if (!valid_state(state, nr, nc)) break()
      if (path[(state[3] - 1) * nr * nc + pos_to_index(state[1:2], nr)] == 1){
        break()
      }
    }
  }
  path
}

score_path <- function(pp_list) {
  x <- map_int(pp_list, ~.[1])
  y <- map_int(pp_list, ~.[2])
  out <- matrix(0, nrow = nrow(mm), ncol = ncol(mm))
  for (i in seq_along(x)){
    out[x[i], y[i]] <- 1
  }
  sum(out)
}

score_path2 <- function(x){
  matrix(x, ncol = 4) %>%
    apply(1, max) %>%
    sum()
}
