# Used a DP method which is ok but its slow for part 2.
# R is slow with recursive functions
# The time taken per problem is very variable and hard to predict.
library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
source("code/day_12_func.R")
txt <- readLines("data/data_12.txt")

lst <- str_split(txt, " ")
recs <- map_chr(lst, ~pluck(., 1))
nums <- map_chr(lst, ~pluck(., 2)) %>%
  map(~as.numeric(str_split_1(., ",")))
nums

# Combinatorial method ------------

sol <- map2_int(recs, nums, ~n_arrange(.x, .y), .progress = TRUE)
sum(sol) # answer

# part 2
# Not practical
recs <- map_chr(lst, ~pluck(., 1)) %>%
  map_chr(~str_c(rep(., 5), collapse = "?"))
nums <- map_chr(lst, ~pluck(., 2)) %>%
  map(~as.numeric(str_split_1(., ","))) %>%
  map(~rep(., 5))
head(recs)
str_length(recs[1])
head(nums)

## n combinations
tb <- tibble(record = recs) %>%
  mutate(nhash = str_count(record, "#")) %>%
  mutate(nqm = map_int(record, ~length(str_which(str_split_1(., ""), "\\?")))) %>%
  mutate(nn = map_int(nums, sum)) %>%
  mutate(n_comb = choose(nqm, nn - nhash))
tb
max(tb$n_comb) # 48620
mean(tb$n_comb)

# DP Method-------

system_time(sol <- map2_int(recs, nums, ~count_arrange(.x, .y), .progress = TRUE))
sum(sol)

nn <- 2
system_time(sol2 <- map2_int(recs[nn], nums[nn], ~count_arrange(.x, .y), .progress = TRUE))
identical(sol, sol2)

# DP decomposition Method-------


recs[17] %>%
  str_remove("\\.+$") %>%
  str_remove("^\\.+") %>%
  str_split_1("\\.+") %>%
  dp_blocks(nums[[3]])
dp_blocks("#", 1)
debugonce(dp_blocks)

nn <- seq_along(recs)
recs2 <- str_remove(recs, "\\.+$") %>%
  str_remove("^\\.+")
system_time(sol <- map2_int(recs2[nn], nums[nn], ~count_arrange(.x, .y), .progress = TRUE))
system_time(sol2 <- map2_int(recs2[nn], nums[nn], ~dp_blocks(str_split_1(.x, "\\.+"), .y), .progress = TRUE))
identical(sol, sol2)
sum(sol2)

# part 2-------------
recs <- map_chr(lst, ~pluck(., 1)) %>%
  map_chr(~str_c(rep(., 5), collapse = "?")) %>%
  str_remove("\\.+$") %>%
  str_remove("^\\.+")
nums <- map_chr(lst, ~pluck(., 2)) %>%
  map(~as.numeric(str_split_1(., ","))) %>%
  map(~rep(., 5))
nn <- seq_along(recs)
nn <- 1
system_time(sol <- map2_int(recs[nn], nums[nn], ~count_arrange(.x, .y), .progress = TRUE))
#system_time(sol2 <- map2_dbl(recs[nn], nums[nn], ~dp_blocks(str_split_1(.x, "\\.+"), .y), .progress = TRUE))
sum(sol2)

tb <- readRDS("out_12.RDS")
sol <- numeric(length(recs))
tt <- numeric(length(recs))
sol <- tb$result
tt <- tb$time
which(sol == 0)
for (i in 523:length(recs)){
  ti <- system_time(sol[i] <- dp_blocks3(str_split_1(recs[i], "\\.+"), nums[[i]]))
  cat(i, "\n")
  if (ti[1] > 1) print(ti)
  tt[i] <- ti[1]
}

max_block <- function(x) {
  str_split_1(x, "\\.+") %>%
    str_length() %>%
    max()
}
tb <- tibble(id = 1:1000, result = sol, time = tt, record = recs, nn = nums) %>%
  mutate(rlen = str_length(recs),
         nlen = map_int(nn, length),
         ntot = map_int(nn, sum)) %>%
  mutate(len_block = map_int(record, max_block)) %>%
  mutate(nhash = str_count(record, "#")) %>%
  mutate(dots = str_detect(record, "\\."))

saveRDS(tb, "out_12.RDS")

# Explore timings -------------
# longest block in a record

arrange(tb, desc(time))
arrange(tb, desc(len_block)) %>%
  select(id, time, nlen, ntot, len_block, record) %>%
  view()
view(select(tb, time, nlen, ntot, len_block, record, dots))
ggplot(tb, aes(log(time), log(result))) +
  geom_point()

ggplot(tb, aes(len_block, log(time))) +
  geom_point()
ggplot(tb, aes(nlen, log(time))) +
  geom_point()

ggplot(tb, aes(nhash, log(time))) +
  geom_point()

ggplot(tb, aes(rlen / ntot, log(time), colour = dots)) +
  geom_point()
