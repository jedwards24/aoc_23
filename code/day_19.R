library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
source("code/day_19_func.R")
txt <- readLines("data/test_19.txt")
txt <- readLines("data/data_19.txt")

inputs <- str_subset(txt, "^\\{")
rules <- str_subset(txt, "^[a-z]")
fnames <- str_extract(rules, "^[a-z]+") %>%
  str_replace("in", "inn")
cond_list <- str_extract_all(rules, ".(<|>)\\d+") %>%
  setNames(fnames)
outcomes <- str_extract_all(rules, "(?<=:)(\\w|,|<|>)*(?=:|\\})")
out_list <- map(outcomes, ~str_split(., ",")) %>%
  map(~map(., ~map(., process_outcome))) %>%
  setNames(fnames)


input_num <- str_remove_all(inputs, "\\=|[a-z]") %>%
  str_remove_all("\\{|\\}") %>%
  str_split(",") %>%
  map(., as.numeric)

identical(map_int(cond_list, length), map_int(out_list, length))

str_count(rules, ":") %>% max()

# Run on inputs
acc <- map_lgl(input_num, ~eval_rule(., cond_list[["inn"]], out_list[["inn"]]))
acc
map_dbl(input_num, sum)[acc] %>%
  sum() #answer


# Part 2---------------
library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
source("code/day_19_func.R")
txt <- readLines("data/test_19.txt")
txt <- readLines("data/data_19.txt")

rules <- str_subset(txt, "^[a-z]")
fnames <- str_extract(rules, "^[a-z]+") %>%
  str_replace("in", "inn")
cond_list <- str_extract_all(rules, ".(<|>)\\d+") %>%
  setNames(fnames)
outcomes <- str_extract_all(rules, "(?<=:)(\\w|,|<|>)*(?=:|\\})")
out_list <- map(outcomes, ~str_split(., ","))

cond_list
out_list %>% unlist %>% str_count("R|A") %>% sum() # should be this many paths

rpaths <- map2(cond_list, out_list, rule_paths) %>%
  setNames(fnames)

rp <- rpaths
while(length(rp) > 1){
  ends <- map(rp, get_ends)
  nms <- names(rp)
  todo <- map_lgl(ends, ~all(. == "R" | . == "A"))
  for (cur in nms[todo]){
    ind <- which(map_lgl(ends, ~any(. == cur)))
    rp[[ind]] <- merge_rule_paths(rp[[ind]], rp[[cur]], names(rp[ind]), cur)

  }
  rp[nms[todo]] <- NULL
}
final_ends <- map_chr(rp[[1]], ~.[length(.)])
final_paths <- map(rp[[1]], ~.[-length(.)])
p_combs <- map_dbl(final_paths, path_combs)

sum(p_combs) == 4000^4 # should be TRUE
sum(p_combs[final_ends == "A"]) # answer
