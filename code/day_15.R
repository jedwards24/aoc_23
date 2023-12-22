# Straightforward. Part 2 was fiddly.
library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
tt <- c("rn=1", "cm-", "qp=3", "cm=2", "qp-", "pc=4", "ot=9", "ab=5", "pc-", "pc=6", "ot=7")
txt <- readLines("data/data_15.txt")
length(txt)
tt <- str_split_1(txt, ",")
head(tt)
hash <- function(x) {
  curr <- 0
  x <- str_split_1(x, "")
  for(i in seq_along(x)){
    curr <- ((curr + asc(x[i])) * 17) %% 256
  }
  curr
}

hash("HASH")
asc <- function(x) {
  strtoi(charToRaw(x),16L)
}

res <- map_dbl(tt, hash)
sum(res) # answer

str_sub(tt, 1, 2) %>% map_int(hash)

# part 2--------------
lab <- str_extract(tt, "^[a-z]*")
hh <- map_int(lab, hash)
eq <- str_detect(tt, "=")
lens <- str_extract(tt, "\\d") %>% as.integer()

boxes <- vector("list", 256)
for ( i in seq_along(tt)){
  bi <- hh[i] + 1
  box_lab <- pluck(boxes, bi, 1)
  if (eq[i]){ # equals
    if (lab[i] %in% box_lab){
      ind <- which(box_lab == lab[i])
      stopifnot(length(ind) == 1)
      pluck(boxes, bi, 2, ind) <- lens[i]
    }else{
      pluck(boxes, bi, 1) <- c(box_lab, lab[i])
      pluck(boxes, bi, 2) <- c(pluck(boxes, bi, 2), lens[i])
    }
  }else{ # minus
    if (lab[i] %in% box_lab){
      ind <- which(box_lab == lab[i])
      stopifnot(length(ind) == 1)
      pluck(boxes, bi, 1) <- box_lab[-ind]
      pluck(boxes, bi, 2) <- pluck(boxes, bi, 2)[-ind]
    }
  }
}
score_box <- function(box, id) {
  x <- pluck(box, 2) #lens
  if (length(x) == 0) return(0)
  sum(id * seq_along(x) * x)
}
boxes[1:3]
map2_dbl(boxes, 1:256, score_box) %>% sum()

map_int(boxes, ~length(pluck(., 1)))
