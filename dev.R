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

beam_path4(mm, c(7, 2, 3)) %>% sum()
mm
system_time(pp2 <- beam_path2(mm))
system_time(pp3 <- beam_path4(mm))
identical(pp2, pp3)
sum(pp3)
is_empty(list())
mark(is_empty(x), length(x) == 0)
x <- list(1:2, 1, 3)
x
x[[1]] <- NULL
x

debugonce(beam_path4)
pp3
