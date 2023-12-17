# Straightforward
library(tidyverse)

dt <- tibble(time = c(40,82,84,92),
             distance = c(233, 1011, 1110, 1487))
dt

race_dist <- function(time, press) {
  press * (time - press)
}

score <- function(time, distance) {
  sum(race_dist(time, seq.int(time)) > distance)
}

dt2 <- dt %>%
  mutate(score = map2_dbl(time, distance, score))

prod(dt2$score)
race_dist(82, 0:82) %>%
  {. > 1011} %>%
  sum() #answer

# part 2-------------

time = 40828492
distance = 233101111101487

score(time, distance) # answer
