# Part 1-----------------

# Collect all maps in a single tibble
map_to_tbl <- function(start, end, text = txt) {
  str_split(txt[start:end], " ") %>%
    map_dfr(~set_names(., c("dest", "source", "len"))) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(d_end = dest + len - 1)
}

# x is a vector, map is a map tbl
map_step <- function(x, map) {
  out <- numeric(length(x))
  map <- map %>%
    mutate(s_end = source + len - 1)
  for (i in seq_along(x)){
    mapi <- filter(map, source <= x[i], s_end >= x[i])
    stopifnot(nrow(mapi) <= 1)
    out[i] <- if (nrow(mapi) == 0)  x[i] else x[i] - mapi$source + mapi$dest
  }
  out
}

# Part 2-----------------

# Return a single map as a tibble
map_to_tbl2 <- function(start, end, text = txt) {
  str_split(txt[start:end], " ") %>%
    map_dfr(~set_names(., c("dest", "source", "len"))) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(s_end = source + len - 1) %>%
    mutate(adjust = dest - source) %>%
    select(source, s_end, adjust)
}

# Seed is a length 2 vector (start finish of a range). map is a tibble
# Returns a list of new seeds (may be more than one).
map_step2 <- function(seed, map) {
  map <- map %>%
    filter(between(source, seed[1], seed[2]) |
             between(s_end, seed[1], seed[2]) |
             (source < seed[1] & s_end > seed[2])) %>%
    arrange(source)
  if (nrow(map) == 0) return(seed)
  out <- list()
  mstart <- map$source
  mend <- map$s_end
  adjust <- map$adjust

  for (i in seq_along(mstart)){
    if (i == 1 && seed[1] < mstart[i]){
      out <- list(c(seed[1], mstart[i] - 1))
    }
    out[[length(out) + 1]]  <- c(max(seed[1], mstart[i]), min(seed[2], mend[i])) + adjust[i]
    if (i == length(mstart)){
      if (seed[2] > mend[i]){
        out[[length(out) + 1]] <- c(mend[i] + 1, seed[2])
      }
    }else{
      if (mend[i] + 1 < mstart[i + 1]){
        out[[length(out) + 1]] <- c(mend[i] + 1, mstart[i + 1] - 1)
      }
    }
  }
  out
}
