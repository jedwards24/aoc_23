library(tidyverse)
library(edwards)
txt <- readLines("data/data_4.txt")
card_list <- str_remove(txt, "^.*: ") %>%
  str_split("\\|")
card_list[[1]]
txt[1]
win_list <- map_chr(card_list, ~pluck(., 1)) %>%
  str_trim() %>%
  str_split(" +") %>%
  map(as.integer)

my_list <- map_chr(card_list, ~pluck(., 2)) %>%
  str_trim() %>%
  str_split(" +") %>%
  map(as.integer)

length(win_list) == length(my_list)
head(my_list)
head(card_list)
my_list[[1]]

matches <- map2_int(win_list, my_list, ~sum(.x %in% .y))
x1 <- win_list[[3]] %>% sort()
x2 <- my_list[[3]] %>% sort()
sum(x1 %in% x2)

pos_matches <- matches[matches >= 1]
sum(2 ^ (pos_matches - 1)) #answer
vcount(matches)

# Part 2---------
cards <- rep(1, length(matches))
for (i in seq_along(matches)){
  if (matches[i] > 0){
    range <- (i+1):(i + matches[i])
    range <- range[range <= length(matches)]
    cards[range] <- cards[range] + cards[i]
  }
}
sum(cards) #answer

tail(cards, 30)
head(cards)
head(matches)
