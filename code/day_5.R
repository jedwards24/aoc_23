# Part 2 not complete - Memory and computational issues
library(tidyverse)
library(edwards)
txt <- readLines("data/data_5.txt")
seeds <- str_split_1(txt[1], " ")[-1] %>%
  as.numeric()

map_to_tbl <- function(start, end, text = txt) {
  str_split(txt[start:end], " ") %>%
    map_dfr(~set_names(., c("dest", "source", "len"))) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(d_end = dest + len - 1)
}

# x is a vector, map is a map tbl
map_step <- function(x, map) {
  out <- numeric(length(x))
  map <- map %>%
    mutate(s_end = source + len - 1)
  for (i in seq_along(x)){
    mapi <- filter(map, source <= x[i], s_end >= x[i])
    stopifnot(nrow(mapi) <= 1)
    out[i] <- if (nrow(mapi) == 0)  x[i] else x[i] - mapi$source + mapi$dest
  }
  out
}


map_locs <- str_which(txt, "map")
ends <- c(map_locs[-1] - 2, length(txt))
maps_loc_tbl <- tibble(text = str_subset(txt, "map"), start = map_locs + 1, end = ends)

map_tbl <- mutate(maps_loc_tbl, tbl = map2(start, end, map_to_tbl))


map_step(seeds, map_tbl$tbl[[1]])

curr <- seeds
for (i in 1 : nrow(map_tbl)){
  curr <- map_step(curr, map_tbl$tbl[[i]])
}
curr
min(curr) # answer

# Part 2 ----------------
seeds
stb <- tibble(start = seeds[seq(1, 19, by = 2)],
              len = seeds[seq(2, 20, by = 2)],
              end = start + len - 1)
min(stb$start)
sum(stb$len)
#s2 = map2(stb$start, stb$len, ~.x:(.x + .y - 1))
object_size_all()

seeds2loc <- function(start, len) {
  curr <- start : (start + len - 1)
  for (i in 1 : nrow(map_tbl)){
    curr <- map_step(curr, map_tbl$tbl[[i]])
  }
  min(curr)
}

seeds2loc(stb$start[1], stb$len[1])
seeds2loc(stb$start[1], 1000)

map_tbl <- mutate(maps_loc_tbl, tbl = map2(start, end, map_to_tbl))

map_step_rev <- function(x, map) {
  out <- numeric(length(x))
  for (i in seq_along(x)){
    mapi <- filter(map, dest <= x[i], d_end >= x[i])
    stopifnot(nrow(mapi) <= 1)
    out[i] <- if (nrow(mapi) == 0)  x[i] else x[i] - mapi$dest + mapi$source
  }
  out
}


curr <- 0:100
for (i in nrow(map_tbl):1){
  curr <- map_step_rev(curr, map_tbl$tbl[[i]])
}
curr

check_seeds <- function(x, tbl = stb) {
    any(stb$start >= x & stb$end <= x)
}

map_lgl(curr, check_seeds)

test_loc <- function(x) {
  curr <- x
  for (i in nrow(map_tbl):1){
    curr <- map_step_rev(curr, map_tbl$tbl[[i]])
  }
#  cat(check_seeds(curr), "\n")
  curr
}

#test_loc(0:100)
map_lgl(101:1000, test_loc)


for (i in 1:1e6){
  if (i %% 10000 == 0) cat(i, "\n")
  if (test_loc(i)) break()
}
i

xx <- 857589555
test_loc(xx) %>% check_seeds()
xx <- 4.2e6
all.equal(test_loc(0) + xx, test_loc(xx))

tt <- map_lgl(test_loc(0) : (test_loc(0) + xx), check_seeds)
bench::mark(map_lgl(1:2e5, check_seeds))
yy <- test_loc(0) + xx
stb
yy >= stb$start
yy <= stb$end

#----------------
x <- map_tbl$tbl[[7]]
x
max(x$source + x$len)
x <- numeric(4e8)
