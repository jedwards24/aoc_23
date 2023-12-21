library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
txt <- readLines("data/data_12.txt")

lst <- str_split(txt, " ")
recs <- map_chr(lst, ~pluck(., 1))
nums <- map_chr(lst, ~pluck(., 2)) %>%
  map(~as.numeric(str_split_1(., ",")))

nn <- seq_along(recs)
recs2 <- str_remove(recs, "\\.+$") %>%
  str_remove("^\\.+")
#system_time(sol <- map2_int(recs2[nn], nums[nn], ~count_arrange(.x, .y), .progress = TRUE))
system_time(sol <- map2_int(recs2[nn], nums[nn], ~dp_blocks(str_split_1(.x, "\\.+"), .y)))
system_time(sol2 <- map2_int(recs2[nn], nums[nn], ~dp_blocks3(str_split_1(.x, "\\.+"), .y)))

sum(sol) == 7361
sum(sol2) == 7361
identical(sol, sol2)
head(sol)
head(sol2)
recs2[6]
nums[[6]]
