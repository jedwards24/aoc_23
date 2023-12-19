library(tidyverse)
library(edwards)
source("functions.R")
txt <- readLines("data/data_12.txt")

lst <- str_split(txt, " ")
recs <- map_chr(lst, ~pluck(., 1))
nums <- map_chr(lst, ~pluck(., 2)) %>%
  map(~as.numeric(str_split_1(., ",")))
nums

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

#----------
unlist(str_match_all(recs[[1]], "\\?+")) %>% str_length()
?str_which
nhash <- str_count(recs[1], "#")
r <- str_split_1(recs[1], "")
r
n <- nums[[1]]
qm <- str_which(r, "\\?")
cm <- combn(length(qm), sum(n) - nhash, simplify = F)
hash_ind <- map(cm, ~qm[.])
head(hash_ind)
rec_to_test <- map_chr(hash_ind, ~replace_qms(r, .))
map_lgl(rec_to_test, test_arrange, num = n) %>% sum()

