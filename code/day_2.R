library(tidyverse)
library(edwards)
txt <- readLines("data/data_2.txt")

x <- txt[1]
x
str_extract(txt, "\\d+")
game_list <- str_remove(txt, "^Game \\d*: ") %>%
  str_split("; ")

tb <- tibble(games = game_list, id = 1: 100) %>%
  unchop(games) %>%
  mutate(red = str_extract(games, "\\d+(?= red)")) %>%
  mutate(blue = str_extract(games, "\\d+(?= blue)")) %>%
  mutate(green = str_extract(games, "\\d+(?= green)")) %>%
  mutate(across(c(red, green, blue), ~replace_na(as.integer(.), 0L)))
count_nas(tb)
tb

res <- tb %>%
  mutate(valid = red <= 12 & green <= 13 & blue <=14) %>%
  group_by(id) %>%
  summarise(across(c(red, green, blue), max), valid = all(valid))
res
filter(res, valid) %>%
  pull(id) %>%
  sum()

# Part 2-------------
res2 <- tb %>%
  group_by(id) %>%
  summarise(across(c(red, green, blue), max)) %>%
  mutate(power = red * green * blue)
res2
sum(res2$power)
