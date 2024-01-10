# Sum all gardens reached in all maps
# x is vector output from quadrant
total_gardens <- function(x, max_steps, lu) {
  map_dbl(max_steps - x, ~garden_count(., lu)) %>%
    sum()
}

# Count gardens reachable from single map
# x is scalar (steps left)
garden_count <- function(x, lu) {
  max(lu$n[lu$steps <= x])
}

# Table of number of gardens reached on a single map starting a corner
# for any given number of steps remaining.
# Even numbered steps only
corner_lu <- function(start, map_mat) {
  mmc <- map_mat
  mmc[mmc == "S"] <- "."
  mmc[start[1], start[2]] <- "S"
  mms <- steps(mmc)
  mms %>%
    as.vector() %>%
    {.[. != "#"]} %>%
    as.integer() %>%
    countv( sort = FALSE) %>%
    filter(value %% 2 == 0) %>%
    mutate(cumn = cumsum(n)) %>%
    select(value, cumn) %>%
    rename(steps = value, n = cumn)
}

# As corner_lu() but for NEWS directions.
# There are two possible starting corners.
# `starts` is a list of two locations
# `start_steps` is a length 2 vector of relative starting steps of the two starts
# ODD NUMBERED STEPS ONLY DEFAULT (change `odd` otherwise).
corner_lu_news <- function(starts, start_steps, map_mat, odd = TRUE) {
  #  rem <- if (odd) 1 else 0
  mmc <- map_mat
  mmc[mmc == "S"] <- "."
  mms_list <- map2(starts, start_steps, ~steps_news(.x, .y, mmc))
  pmin(mms_list[[1]], mms_list[[2]]) %>%
    as.vector() %>%
    countv( sort = FALSE) %>%
    filter(value %% 2 == 0) %>%
    mutate(cumn = cumsum(n)) %>%
    select(value, cumn) %>%
    rename(steps = value, n = cumn)
}
