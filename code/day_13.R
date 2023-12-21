# Fairly quick to do.
# Missed reading one problem since there was not a blank line at the end of the data when I thought
# there was from looking at the text file.

library(tidyverse)
library(edwards)
source("functions.R")
txt <- readLines("data/data_13.txt")
tlist <- split_text(c(txt, ""))
mlist <- map(tlist, text_to_matrix)

x <- tlist[[1]]
x
text_to_matrix(x) %>% dim()

# Mirror is to right of col `pos`
check_symmetry_col <- function(x, pos) {
  end <- max(1, pos - (ncol(x) - pos) + 1)
  for (i in pos:end){
    if (any(x[, i] != x[, 2 * pos - i + 1])) return(FALSE)
  }
  TRUE
}

# Mirror is below row `pos`
check_symmetry_row <- function(x, pos) {
  end <- max(1, pos - (nrow(x) - pos) + 1)
  for (i in pos:end){
    if (any(x[i, ] != x[2 * pos - i + 1, ])) return(FALSE)
  }
  TRUE
}

rsymm <- function(x) {
  y <- map_lgl(1:(nrow(x) - 1), ~check_symmetry_row(x, .))
  max(which(y), 0)
}

csymm <- function(x) {
  y <- map_lgl(1:(ncol(x) - 1), ~check_symmetry_col(x, .))
  max(which(y), 0)
}
x <- mlist[[99]]
rsymm(x)
csymm(x)
x

rs <- map_int(mlist, rsymm)
cs <- map_int(mlist, csymm)
sum(cs) + sum(rs) * 100 #answer

tb <- tibble(id = seq_along(mlist), rs, cs)
tail(tb)

# part 2------------

near_equal <- function(x, y) {
  sum(x == y) == length(x) - 1
}

# Mirror is to right of col `pos`
check_symmetry_col <- function(x, pos) {
  end <- max(1, pos - (ncol(x) - pos) + 1)
  ne <- 0
  for (i in pos:end){
    mismatch <- any(x[, i] != x[, 2 * pos - i + 1])
    if (mismatch){
      ne_now <- near_equal(x[, i], x[, 2 * pos - i + 1])
      if (ne == 0 && ne_now){
        ne <- 1
      }else{
        return(FALSE)
      }
    }
  }
  if (ne == 1) TRUE else FALSE
}

# Mirror is below row `pos`
check_symmetry_row <- function(x, pos) {
  end <- max(1, pos - (nrow(x) - pos) + 1)
  ne <- 0
  for (i in pos:end){
    mismatch <- any(x[i, ] != x[2 * pos - i + 1, ])
    if (mismatch){
      ne_now <- near_equal(x[i, ], x[2 * pos - i + 1, ])
      if (ne == 0 && ne_now){
        ne <- 1
      }else{
        return(FALSE)
      }
    }
  }
  if (ne == 1) TRUE else FALSE
}

txt <- readLines("data/data_13.txt")
tlist <- split_text(c(txt, ""))
mlist <- map(tlist, text_to_matrix)

rs <- map_int(mlist, rsymm)
cs <- map_int(mlist, csymm)
sum(cs) + sum(rs) * 100 # answer
tb <- tibble(id = seq_along(mlist), rs, cs)
tb
mlist[[1]]
