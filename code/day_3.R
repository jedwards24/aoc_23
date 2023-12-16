# I went tidyverse with this which felt a bit cumbersome but it worked first time

library(tidyverse)
library(edwards)
txt <- readLines("data/data_3.txt")

# symbols in data
syms <- str_split(txt,"") %>%
  unlist() %>%
  unique() %>%
  setdiff(c(".", as.character(0:9)))
syms
sym_pat <- str_c(syms, collapse = "|")
head(txt)

num_locs <- str_locate_all(txt, "\\d+") %>%
  map(as_tibble)
num_locs[[1]]
txt[140]

# Numbers are 1,2, or 3 digits
str_extract_all(txt, "\\d+") %>%
  unlist() %>%
  str_length() %>%
  vcount()

locs <- enframe(num_locs, name = "row", value = "data") %>%
  unnest(data) %>%
  mutate(num = unlist(str_extract_all(txt, "\\d+")))
locs
filter(locs, (end - start) <= 1)

# Returns true is number at row/start/end/ has an adjacent symbol
check_syms <- function(txt, row, start, end, symbols = syms, ...) {
  rows_check <- intersect((row - 1):(row + 1), 1:length(txt))
  stopifnot(length(rows_check) > 0)
  for (i in rows_check){
    text_check <- str_sub(txt[i], start - 1, end + 1)
    if (any(str_split_1(text_check, "") %in% symbols)) return(TRUE)
  }
  FALSE
}

locs
check_syms(txt, 1, 57, 59)
txt[141]
head(txt)

locs %>%
  rowwise() %>%
  mutate(by_sym = pmap_lgl(list(row, start, end), check_syms))

locs2 <- mutate(locs, by_sym = pmap_lgl(locs, check_syms, txt = txt))
locs2

locs2 %>%
  filter(by_sym) %>%
  pull(num) %>%
  as.integer() %>%
  sum()

# Part 2------------
star_locs <- str_locate_all(txt, "\\*{1}") %>%
  map(as_tibble) %>%
  enframe(name = "row", value = "data") %>%
  unnest(data) %>%
  rename(star_row = row, loc = start) %>%
  select(-end)
star_locs
locs

# Returns multiple of two numbers adjacent to * at star_row/loc or 0 otherwise.
gear_ratio <- function(star_row, loc, ...) {
  adj <- locs %>%
    filter(abs(row - star_row) <= 1) %>%
    filter(loc >= (start - 1), loc <= (end + 1)) %>%
    pull(num) %>%
    as.integer()
  if (length(adj) == 2) return(adj[1] * adj[2])
  return(0)
}
gear_ratio(2, 5)

bench::system_time(star_locs2 <- mutate(star_locs, gr = pmap_int(star_locs, gear_ratio)))
star_locs2
vcount(star_locs2$gr, sort = T)
sum(star_locs2$gr)
