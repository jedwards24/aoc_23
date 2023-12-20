# Messy.
# Used a DP method which is ok but its slow for part 2.
library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
txt <- readLines("data/data_12.txt")

lst <- str_split(txt, " ")
recs <- map_chr(lst, ~pluck(., 1))
nums <- map_chr(lst, ~pluck(., 2)) %>%
  map(~as.numeric(str_split_1(., ",")))
nums

# Combinatorial method ------------

# Get number of valid arrangements for record `rec` and
# `num` - groups of damaged springs `
n_arrange <- function(rec, num) {
  nhash <- str_count(rec, "#")
  rr <- str_split_1(rec, "")
  qm <- str_which(rr, "\\?")
  cm <- combn(length(qm), sum(num) - nhash, simplify = FALSE)
  hash_ind <- map(cm, ~qm[.])
  rec_to_test <- map_chr(hash_ind, ~replace_qms(rr, .))
  sum(map_lgl(rec_to_test, test_arrange, num = num))
}

test_arrange <- function(rec, num) {
  lens <- unlist(str_match_all(rec, "\\#+")) %>%
    str_length()
  if (length(lens) != length(num)) return (FALSE)
  all(lens == num)
}

# x is split record. Returns a collapsed string
replace_qms <- function(x, hash_ind) {
  x[hash_ind] <- "#"
  str_flatten(str_replace(x, "\\?", "."))
}

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
# rr is record, nn is lengths of contiguous blocks (num)
count_arrange <- function(rr, nn) {
  rr <- str_remove(rr, "^\\.+")
  if (length(nn) == 0){
    if (str_detect(rr, "\\#")){
      return(0)
    }
    return(1)
  }
  if (str_length(rr) < sum(nn) + length(nn) - 1) return(0L)
  valid_now <- !(str_detect(str_sub(rr, 1, nn[1]), "\\.") ||
    str_sub(rr, nn[1] + 1, nn[1] + 1) == "#")
  if (str_sub(rr, 1, 1) == "#"){
    if (valid_now){
      return(count_arrange(str_sub(rr, nn[1] + 2), nn[-1]))
    }
    return(0)
  }
  if (!valid_now){
    return(count_arrange(str_sub(rr, 2), nn))
  }
  return(count_arrange(str_sub(rr, nn[1] + 2), nn[-1]) +
           count_arrange(str_sub(rr, 2), nn))
}

system_time(sol <- map2_int(recs, nums, ~count_arrange(.x, .y), .progress = TRUE))
sum(sol)

nn <- 2
system_time(sol2 <- map2_int(recs[nn], nums[nn], ~count_arrange(.x, .y), .progress = TRUE))
identical(sol, sol2)

# DP decomposition Method-------

# rr is record, nn is lengths of contiguous blocks (num)
# Similar to count_arrange() but with no dots in rr input
count_arrange2 <- function(rr, nn) {
  if (length(nn) == 0){
    if (str_detect(rr, "\\#")){
      return(0)
    }
    return(1)
  }
  if (length(nn) == 1 && str_length(rr) == nn) return(1)
  if (str_length(rr) < sum(nn) + length(nn) - 1) return(0)
  if (!str_detect(rr, "\\#")){
    return(choose(str_length(rr) + 1 - sum(nn), length(nn)))
  }
  if (str_sub(rr, nn[1] + 1, nn[1] + 1) != "#"){ # check is valid
    if (str_sub(rr, 1, 1) == "#"){
      return(count_arrange2(str_sub(rr, nn[1] + 2), nn[-1]))
    }
    return(count_arrange2(str_sub(rr, nn[1] + 2), nn[-1]) +
             count_arrange2(str_sub(rr, 2), nn))
  }
  if (str_sub(rr, 1, 1) == "#") return(0)
  return(count_arrange2(str_sub(rr, 2), nn))
}

# `blocks` is a record split by dots.
dp_blocks <- function(blocks, nn) {
  if (length(nn) == 0){
    if (str_detect(str_flatten(blocks), "\\#")){
      return(0)
    }
    return(1)
  }
  if (length(blocks) == 0) return(0)
  total <- 0
  if (length(blocks) == 1) return(count_arrange2(blocks[1], nn))
  if (!str_detect(blocks[1], "\\#")){
    total <- total + dp_blocks(blocks[-1], nn)
  }
  for (i in 1 : length(nn)){
    nset <- nn[1:i]
    if (str_length(blocks[1]) < sum(nset) + length(nset) - 1) break()
    total <- total +
      dp_blocks(blocks[-1], nn[-c(1:i)]) *
      count_arrange2(blocks[1], nset)
  }
  total
}

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

# part 2
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
nums[1:4]
recs[1:4]

sol <- numeric(length(recs))
tt <- numeric(length(recs))
for (i in 100){
  tt[i] <- system_time(sol[i] <- dp_blocks(str_split_1(recs[i], "\\.+"), nums[[i]]))[1]
  cat(i, "\n")
  print(tt[i])
}
head(sol)
n <- 556
recs[n]
nums[[n]]
sum(nums[[n]])
str_length(recs[n])

enframe(tt) %>%
  arrange(desc(value)) %>%
  print(n = 30)
