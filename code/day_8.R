library(tidyverse)
library(edwards)
txt <- readLines("data/data_8.txt")
txt[2]
direc <- txt[1] %>%
  str_split_1("")
direc_num <- if_else(direc == "L", 1L, 2L)

start <- str_extract(txt[-c(1, 2)], "^[A-Z]{3}")
tail(start)
left <- str_extract(txt[-c(1, 2)], "[A-Z]{3}(?=\\,)")
right <- str_extract(txt[-c(1, 2)], "[A-Z]{3}(?=\\))")
dir_mat <- cbind(left, right)

loc_list <- list()
loc <- character(length(direc))

new <- "AAA"
done <- FALSE
moves <- 0
j = 1
while (!done){
  for(i in seq_along(direc_num)){
    new <- dir_mat[which(start == new), direc_num[i]]
    moves <- moves + 1
    if (new == "ZZZ"){
      done <- TRUE
      break()
    }
  }
  if (j %% 100 == 0)cat(j)
  j = j + 1
}
moves # answer

# part 2---------------
# Incrementing - could take a very long time. See fast alternative below
str_subset(start, "Z$")
new <- str_subset(start, "A$")
done <- FALSE
moves <- 0
j = 1
while (!done){
  for(i in seq_along(direc_num)){
    new <- dir_mat[match(new, start), direc_num[i]]
    moves <- moves + 1
    if (all(str_sub(new, 3, 3) == "Z")){
      done <- TRUE
      break()
    }
  }
  if (j %% 1000 == 0)cat(j, " ")
  j = j + 1
}
loc

i = 1

dir_mat[match(new, start), direc_num[i]]
match(new, start)

#------------
# Using LCM
library(numbers)
lcm_vector <- function(x) Reduce(LCM, x)

tt <- str_subset(start, "A$")
mm <- c()
for (k in 1: 6){
  new <- tt[k]
  done <- FALSE
  moves <- 0
  while (!done){
    for(i in seq_along(direc_num)){
      new <- dir_mat[match(new, start), direc_num[i]]
      moves <- moves + 1
      if (str_sub(new, 3, 3) == "Z"){
        done <- TRUE
        break()
      }
    }
  }
  moves
  mm[k] <- moves
}
mm
LCM(mm[1], mm[2])
lcm_vector(mm) #answer
