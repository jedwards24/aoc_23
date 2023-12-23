# switch clearly faster than if statements

mirrorb <- function(x) {
  if (x[3] == 1) return(x + c(0, -1, 3))
  if (x[3] == 2) return(x + c(1, 0, 1))
  if (x[3] == 3) return(x + c(0, 1, -1))
  if (x[3] == 4) return(x + c(-1, 0, -3))
}

mirrorb2 <- function(x) {
  switch(x[3],
         x + c(0, -1, 3),
         x + c(1, 0, 1),
         x + c(0, 1, -1),
         x + c(-1, 0, -3)
  )
}

x <- c(5, 8, 4)
mirrorb(x)
mirrorb2(x)
mark(mirrorb(x), mirrorb2(x))
