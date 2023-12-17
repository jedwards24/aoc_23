# Poker hand reading
library(tidyverse)
library(edwards)
txt <- readLines("data/data_7.txt")
txt
lst <- str_split(txt, " ")
hands <- map_chr(lst, ~pluck(., 1))
bids <- map_chr(lst, ~pluck(., 2)) %>% as.integer()
hands

cards <- c(2:9, "T", "J", "Q", "K", "A")

ranks <- str_split(hands, "") %>%
  map(~match(., cards))

max2 <- function(x) {
  c(max(x), max(x[-which.max(x)]))
}

# input is numeric ranks
score_hand <- function(x) {
  class_hand(x) * 14^5 + sum(x * 14^(4:0))
}

class_hand <- function(x) {
  best <- max2(tabulate(x))
  if (best[1] >= 4) return(best[1] + 1)
  if (all(best == c(3, 2))) return(4)
  if (best[1] == 3) return(3)
  if (all(best == c(2, 2))) return(2)
  if (best[1] == 2) return(1)
  0
}

scores <- map_int(ranks, score_hand)
head(scores)
head(hands)

res <- tibble(hands, bids, scores, rank1 = hrank) %>%
  mutate(rank = row_number(scores))
  arrange(rank)
res
sum(res$rank * res$bids) # answer
count_n(res, scores)
tail(res)

# part2 -------------
class_hand <- function(x) {
  if (all(x == 1)) return(6)
  counts <- tabulate(x)
  rank_most <- which.max(counts[-1])[1] + 1
  x[x == 1] <- rank_most
  best <- max2(tabulate(x))
  if (best[1] >= 4) return(best[1] + 1)
  if (all(best == c(3, 2))) return(4)
  if (best[1] == 3) return(3)
  if (all(best == c(2, 2))) return(2)
  if (best[1] == 2) return(1)
  0
}

cards <- c("J", 2:9, "T", "Q", "K", "A")
ranks <- str_split(hands, "") %>%
  map(~match(., cards))

scores <- map_int(ranks, score_hand)

res <- tibble(hands, bids, scores) %>%
  mutate(rank = row_number(scores)) %>%
  arrange(desc(rank))
res
sum(res$rank * res$bids) # answer
