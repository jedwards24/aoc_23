# Part 2 was annoying as the test data had features that made it more difficult than the problem
# data and I spent time solving the test data, which I then did not need

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
# Could be used on test set - 100 takes 5.4s, 500 takes 10 mins
locs <- which(mm == "S", arr.ind = TRUE)
n_steps <- 200
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

# Test set solution-------------
# steps to each corner on next diagonal map from start
mms <- steps(mm)
nr <- nrow(mm)
nc <- ncol(mm)
corners <- c(mms[1, 1], mms[1, nc], mms[nr, nc], mms[nr, 1]) %>%
  as.integer() %>%
  setNames(c("nw", "ne", "se", "sw"))
corners_start <- corners + 2

# Lookup tables for a single map starting in a corner
# Direction are in direction of travel so corner positions are not the same as `corners`
corn_locs <- list(nw = c(nr, nc),
                  ne = c(nr, 1),
                  se = c(1, 1),
                  sw = c(1, nc))

corn_locs_news <- list(n = list(c(nr, 1), c(nr, nc)),
                       e = list(c(1, 1), c(nr, 1)),
                       s = list(c(1, nc), c(1, 1)),
                       w = list(c(1, nc), c(nr, nc)))
corners_start_news <- news_starts(corners)
rel_start <- map(corners_start_news, ~. - min(.))

# Run
n_steps <- 5000
odd_flag <- if (n_steps %% 2 == 1) TRUE else FALSE

system_time(clu_list <- map(corn_locs, corner_lu, map_mat = mm))
system_time(news_lu_list <- map2(corn_locs_news, rel_start, ~corner_lu_news(.x, .y, mm)))
q_list <- map(corners_start, ~quadrant(., n_steps, nr, nc), .progress = TRUE)
ngard_q <- map2_dbl(q_list, clu_list,
                    ~total_gardens(.x, n_steps, .y, odd = odd_flag),
                    .progress = TRUE)

line_list <- map_dbl(corners_start_news, min) %>%
  map(~news_line(., n_steps, nr)) # ONLY IF nr == nc
ngard_news <- map2_dbl(line_list, news_lu_list,
                       ~total_gardens(.x, n_steps, .y, odd = odd_flag),
                       .progress = TRUE)
sum(ngard_news) + sum(ngard_q) + garden_count_start(mms, odd = odd_flag) # answer

# Solution for problem data----------------
# Using map structure of part 2 (news is direct)
# I hard coded the start locations

## setup
# steps to each corner on next diagonal map from start
mms <- steps(mm)
nr <- nrow(mm)
nc <- ncol(mm)
corners <- c(mms[1, 1], mms[1, nc], mms[nr, nc], mms[nr, 1]) %>%
  as.integer() %>%
  setNames(c("nw", "ne", "se", "sw"))
corners_start <- corners + 2

# Lookup tables for a single map starting in a corner
# Direction are in direction of travel so corner positions are not the same as `corners`
corn_locs <- list(nw = c(nr, nc),
                  ne = c(nr, 1),
                  se = c(1, 1),
                  sw = c(1, nc))

news_start <- c(n = mms[1, 66], e = mms[66, nc], s = mms[nr, 66], w = mms[66, 1]) %>%
  as.integer() # all 65 so 66 to start next
news_locs <- list(n = c(nr, 66), e = c(66, 1), s = c(1, 66), w = c(66, nc))

# Run
n_steps <- 26501365
stopifnot(nr == nc)
odd_flag <- if (n_steps %% 2 == 1) TRUE else FALSE

system_time(clu_list <- map(corn_locs, corner_lu, map_mat = mm))
system_time(news_lu_list <- map(news_locs, ~corner_lu(.x, mm)))

q_list <- map(corners_start, ~quadrant(., n_steps, nr, nc), .progress = TRUE)
ngard_q <- map2_dbl(q_list, clu_list,
                    ~total_gardens(.x, n_steps, .y, odd = odd_flag),
                    .progress = TRUE)

line_news <- news_line(66, n_steps, nr) # 66 is hardcoded because all directions are the same
ngard_news <- map_dbl(news_lu_list,
                       ~total_gardens(line_news, n_steps, ., odd = odd_flag),
                       .progress = TRUE)
sum(ngard_news) + sum(ngard_q) + garden_count_start(mms, odd = odd_flag) # answer
