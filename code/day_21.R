library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
source("code/day_21_func.R")
txt <- readLines("data/test_21.txt")
txt <- readLines("data/data_21.txt")
mm <- text_to_matrix(txt)
garden <- mm != "#"
#mm <- matrix(as.numeric(mm), nrow = nrow(mm))
start <- which(mm == "S", arr.ind = TRUE) %>%
  as.vector()

neighbours(start)
neighbours(start) %>%
  apply(1, valid_move)
mm

neighbours(start) %>%
  apply(1, neighbours, simplify = F) %>%
  do.call(rbind, .)

locs <- which(mm == "S", arr.ind = TRUE)
n_steps <- 20
for (i in 1:n_steps){
  new <- locs %>%
    apply(1, neighbours, simplify = F) %>%
    do.call(rbind, .)
  valid <- apply(new, 1, valid_move)
  locs <- new[valid, ] %>%
    as_tibble() %>%
    distinct() %>%
    as.matrix()
  cat(i)
}
locs
nrow(locs) # Answer

# visalise visited states
mm2 <- mm
for (i in 1: nrow(locs)){
  mm2[locs[i, 1], locs[i, 2]] <- "a"
}
mm2

# Part 2--------------
# Try same method as part 1 - works but won't scale well
txt
mm
garden

locs <- which(mm == "S", arr.ind = TRUE)
n_steps <- 500
system_time(
  for (i in 1:n_steps){
    new <- locs %>%
      apply(1, neighbours, simplify = F) %>%
      do.call(rbind, .)
    rocks <- cbind(mod2(new[, 1], nrow(mm)), mod2(new[, 2], ncol(mm)))
    valid <- apply(rocks, 1, valid_move2)
    locs <- new[valid, ] %>%
      as_tibble() %>%
      distinct() %>%
      as.matrix(rownames.force = FALSE)
    if (i %% 100 == 0) cat(i, " ")
  }
)
nrow(locs)
locs
# 100 takes 5.4s, 500 takes 10 mins

# *** Solution sketch ***
#
# If the total number of steps is an odd number then a location will be reachable iff it is visited
# after an odd number of steps (similar rule with an even end).
# Work with an even number of steps, adjusting the starting locations if total steps is odd.
# I do not need to revisit previously visited states but searching through a large number of previous
# steps will be inefficient
#
# Use the repeating pattern of rocks:
# The border of both maps contains no rocks
# So I need:
# - steps to reach each loc on a single map from each border location
# - steps to each loc from the start.
# Actually just need to know what states on boundary are reached from start in an even number of steps,
# then work from each of these.
# Although, the pattern on neighbouring maps will be different so will need all border locs as starts.

# Try a 3x3 map
start <- which(mm == "S", arr.ind = TRUE) %>%
  as.vector()

mm2 <- rbind(mm, mm, mm)
mm3 <- cbind(mm2, mm2, mm2)
mm3[mm3 == "S"] <- "."
mm3[start[1] + nrow(mm), start[2] + ncol(mm)] <- "S"
which(mm3 == "S", arr.ind = T)
mms3 <- steps(mm3)

mms[1:11, 1:11]
mms[12:22, 1:11] # W
mms[12:22, 23:33] # E
mms[1:11, 12:22] # N
mms[23:33, 12:22] # S

# Single map
mms <- steps(mm)
str_extract_all(mms, "\\d+") %>%
  unlist() %>%
  as.integer() %>%
  max()


which(mm == "#") %>% length()

# steps to each corner on next diagonal map from start
nr <- nrow(mms)
nc <- ncol(mms)
corners <- c(mms[nr, nc], mms[nr, 1], mms[1, 1], mms[1, nc]) %>%
  as.integer() %>%
  setNames(c("tl", "tr", "br", "bl")) %>%
  {.+2}

# Lookup tables for a single map starting in a corner
corn_locs <- list(tl = c(1, 1),
                  tr = c(1, nc),
                  br = c(nr, nc),
                  bl = c(nr, 1))
clu_list <- map(corn_locs, corner_lu, map_mat = mm)

# Need:
#  - steps to enter new map n each of 8 direction.
#  - steps to enter new map from starting edge/corner in each direction.
# Matrix with one location for each map. Count cumulative steps to enter each map.
# Calculate number visited at end maps.
#
q1 <- quadrant(12, 100, 11, 11)

total_gardens(q1, 100, clu_list[[1]]) * 4

# NEWS directions
# Single straight line of maps
# Assume shortest traverse line passes through a corner (true on test data)
#
# End map is tricky. Can I just look at starting from up to 2 corners?
# Redo corner_lu() to take minimum of two starting corners
mms
news_lu = list(w = mms3[12:22, 1:11],
               e = mms3[12:22, 23:33],
               n = mms3[1:11, 12:22],
               s = mms3[23:33, 12:22])
news_line(11, 100, nr)
