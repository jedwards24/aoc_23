# Neighbours (news) of x
# x is length 2 vector
# Returns matrix 4x2 matrix
neighbours <- function(x) {
  cbind(c(x[1] - 1, x[1] + 1, x[1], x[1]),
        c(x[2], x[2], x[2] - 1, x[2] + 1))
}

# Is x a valid location given map `mat`
# Valid if in the map and not a rock
# mat is a logical matrix (TRUE if garden)
valid_move <- function(x, mat = garden) {
  x[1] >= 1 && x[1] <= nrow(mat) &&
    x[2] >= 1 && x[2] <= ncol(mat) &&
    mat[x[1], x[2]]
}

# Just checks
valid_move2 <- function(x, mat = garden) {
  mat[x[1], x[2]]
}

# modulo division but with zero replaced by divisor
mod2 <- function(x, y) {
  out <- x %% y
  out[out == 0] <- y
  out
}

# Given map `map_mat` this explores the map until all accessible
# gardens have been visited. Each garden is replaced by the number of steps
# from S that it took to reach it.
steps <- function(map_mat) {
  gardeni <- map_mat != "#"
  locs <- which(map_mat == "S", arr.ind = TRUE)
  mmi <- map_mat
  mmi[locs[1], locs[2]] <- 0
  gardeni[locs[1], locs[2]] <- FALSE
  n_steps <- nrow(mmi) * ncol(mmi)
  for (i in 1:n_steps){
    new <- locs %>%
      apply(1, neighbours, simplify = FALSE) %>%
      do.call(rbind, .)
    valid <- apply(new, 1, valid_move, mat = gardeni)
    if (!any(valid)) break()
    locs <- new[valid, , drop = FALSE] %>%
      as_tibble() %>%
      distinct() %>%
      as.matrix(rownames.force = FALSE)
    for (j in 1:nrow(locs)){
      mmi[locs[j, 1], locs[j, 2]] <- i
      gardeni[locs[j, 1], locs[j, 2]] <- FALSE
    }
#    if (i %% 100 == 0) cat(i, " ")
  }
  mmi
}

# Returns a vector of the number of steps to reach any map
# starting from the corner of a map after `start_steps` steps.
# On;ly maps reached in no more than max_steps are included.
# nr, nc is the nrow, ncol of each map
# A numeric vector is returned with one number for each map.
quadrant <- function(start_steps, max_steps, nr, nc) {
  out <- c()
  for (i in 0:(max_steps / nc)){
    if (start_steps + i * nc > max_steps) break()
    cur <- seq(start_steps + i * nc, max_steps, by = nr)
    out <- c(out, cur)
  }
  out
}

# Returns a vector of the number of steps to reach any map
# moving in a straight line N, E, W, or S, and
# starting from the edge of a map after `start_steps` steps.
# nn is dimension of map in direction of travel.
news_line <- function(start_steps, max_steps, nn) {
  if (start_steps > max_steps) return (c())
  seq(start_steps, max_steps, by = nn)
}

# Sum all gardens reached in all maps
# x is vector output from quadrant
total_gardens <- function(x, max_steps, lu, odd = FALSE) {
  map_dbl(x, ~garden_count(., lu, max_steps, odd)) %>%
    sum()
}

# x is n steps to reach start of map
# lu is vector of steps to reach each garden in map starting from 0
garden_count <- function(x, lu, max_steps, odd = FALSE) {
  rem <- if (odd) 1L else 0L
  steps <- lu + x
  sum(steps <= max_steps & steps %% 2L == rem)
}

# Table of number of gardens reached on a single map starting a corner
# for any given number of steps remaining.
# Even numbered steps only
corner_lu <- function(start, map_mat) {
  mmc <- map_mat
  mmc[mmc == "S"] <- "."
  mmc[start[1], start[2]] <- "S"
  out <- steps(mmc)
  as.integer(out[out != "#" & out != "."])
}

# As corner_lu() but for NEWS directions.
# There are two possible starting corners.
# `starts` is a list of two locations
# `start_steps` is a length 2 vector of relative starting steps of the two starts
corner_lu_news <- function(starts, start_steps, map_mat) {
  mmc <- map_mat
  mmc[mmc == "S"] <- "."
  mms_list <- map2(starts, start_steps, ~steps_news(.x, .y, mmc))
  pmin(mms_list[[1]], mms_list[[2]]) %>%
    as.vector() %>%
    {.[!is.na(.)]}
}

# for debugging
news_lu_map <- function(starts, start_steps, map_mat) {
  mmc <- map_mat
  mmc[mmc == "S"] <- "."
  mms_list <- map2(starts, start_steps, ~steps_news(.x, .y, mmc))
  pmin(mms_list[[1]], mms_list[[2]])
}

# Helper for corner_lu_news
steps_news <- function(start, rel_steps, map_mat) {
  map_mat[start[1], start[2]] <- "S"
  out <- steps(map_mat)
  out[which(out == "#" | out == ".")] <- NA
  matrix(as.integer(out), nrow = nrow(out), ncol = ncol(out)) + rel_steps
}

# Count gardens reached in single map
# (used for starting map)
# x is output of steps()
garden_count_start <- function(x, odd = FALSE) {
  rem <- if (odd) 1L else 0L
  vv <- x[x != "#" & x != "."] %>%
    as.integer()
  length(vv[vv %% 2 == rem])
}

# helper for debugging
# Count gardens reached on single map in no more than n_steps
# Even only unless odd = TRUE
garden_count_single <- function(x, n_steps, odd = FALSE) {
  rem <- if (odd) 1L else 0L
  vv <- x[x != "#"] %>%
    as.integer()
  length(vv[vv <= n_steps & vv %% 2 == rem])
}

# Create list of starting corners for NEWS moves
# x is corn_locs
locs_news <- function(x) {
  dirs <- c("n", "e", "s", "w")
  out <- vector("list", length(dirs))
  for (i in seq_along(dirs)){
    out[[i]] <- x[str_detect(names(x), dirs[i])]
  }
  out
}

# Create list of start for each news starting location
# x is `corners`
news_starts <- function(x) {
  dirs <- c("n", "e", "s", "w")
  out <- vector("list", length(dirs))
  for (i in seq_along(dirs)){
    out[[i]] <- x[str_detect(names(x), dirs[i])] + 1
  }
  out
}
