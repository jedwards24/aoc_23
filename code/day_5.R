# Part 2 not complete - Memory and computational issues
library(tidyverse)
library(edwards)
txt <- readLines("data/data_5.txt")
txt <- readLines("data/test_5.txt")
seeds <- str_split_1(txt[1], " ")[-1] %>%
  as.numeric()

map_locs <- str_which(txt, "map")
ends <- c(map_locs[-1] - 2, length(txt))
maps_loc_tbl <- tibble(text = str_subset(txt, "map"), start = map_locs + 1, end = ends)

map_tbl <- mutate(maps_loc_tbl, tbl = map2(start, end, map_to_tbl))

curr <- seeds
for (i in 1 : nrow(map_tbl)){
  curr <- map_step(curr, map_tbl$tbl[[i]])
}
curr
min(curr) # answer

# Part 2 ----------------
# Later note: I think the map step from part 1 can be used, modified so it works with a range which
# may be split into multiple ranges by the step.
# Actual vectors are too big so use a start and end for each range.
library(tidyverse)
library(edwards)
txt <- readLines("data/data_5.txt")
txt <- readLines("data/test_5.txt")
seeds <- str_split_1(txt[1], " ")[-1] %>%
  as.numeric()

map_locs <- str_which(txt, "map")
ends <- c(map_locs[-1] - 2, length(txt))
maps_loc_tbl <- tibble(text = str_subset(txt, "map"), start = map_locs + 1, end = ends)

map_list <- map2(maps_loc_tbl$start, maps_loc_tbl$end, map_to_tbl2)
stb <- tibble(start = seeds[seq(1, length(seeds) - 1, by = 2)],
              len = seeds[seq(2, length(seeds), by = 2)],
              end = start + len - 1)
seed_list <- map2(stb$start, stb$end, ~c(.x, .y))

# all maps
curr <- seed_list
for (i in seq_along(map_list)){
  curr <- map(curr, ~map_step2(., map_list[[i]])) %>%
    lmap_if(is.list, flatten)
}
curr
out
map_dbl(curr, ~.[1]) %>% min() # Answer
