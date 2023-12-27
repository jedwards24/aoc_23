
library(tidyverse)
library(edwards)
library(bench)
library(sf)
source("functions.R")
test <- T
txt <- readLines("data/data_18.txt")
if (test) txt <- readLines("data/test_18.txt")

tb <- tibble(dir = str_sub(txt, 1, 1),
       len = as.numeric(str_extract(txt, "(?<= )\\d+")))
count_nas(tb)
tb

# Estimate size of matrix needed
tb2 <- tb %>%
  mutate(vert = case_when(
    dir == "U" ~ -len,
    dir == "D" ~ len,
    TRUE ~ 0
  )) %>%
  mutate(horiz = case_when(
    dir == "R" ~ len,
    dir == "L" ~ -len,
    TRUE ~ 0
  )) %>%
  mutate(cv = cumsum(vert),
         ch = cumsum(horiz))
summary(tb2$cv)
summary(tb2$ch)

# Run-------
move <- list("R" = c(0, 1), "L" = c(0, -1), "U" = c(-1, 0), "D" = c(1, 0))
d_vec <- tb$dir
len_vec <- tb$len
mm <- matrix(".", nrow = 450, ncol = 400)
curr <- c(400, 100)
verts <- matrix(NA, length(d_vec), 2)

if (test){
  mm <- matrix(".", nrow = 12, ncol = 12)
  curr <- c(2, 2)
  verts <- matrix(NA, length(d_vec), 2)
}
for (i in seq_along(d_vec)){
  verts[i, ] <- curr
  end <- curr + len_vec[i] * move[[d_vec[i]]]
  mm[curr[1]:end[1], curr[2]:end[2]] <- "#"
  curr <- end
}
verts <- rbind(verts, verts[1, ])

# The matrix I used has space around the path on all sides so I can do the fill from the top corner
# but this will not work generally
flood_fill <- function(mm) {
  nr <- nrow(mm)
  nc <- ncol(mm)
  todo <- matrix(FALSE, nr, nc)
  todo[1, 1] <- TRUE
  while (sum(todo) > 0){
    cur <- which(todo, arr.ind = TRUE)[1, ]
    y <- cur[1]
    x <- cur[2]
    todo[y, x] <- FALSE
    hh <- c(0, which(mm[y, ] == "#"), nc + 1)
    fill <- (max(hh[hh < x]) + 1) : (min(hh[hh > x]) - 1)
    mm[y, fill] <- "*"
    todo[y, fill] <- FALSE
    if (y < nr){
      next_row <- intersect(which(mm[y + 1, ] == "."), fill)
      todo[y + 1, next_row] <- TRUE
    }
    if (y > 1){
      prev_row <- intersect(which(mm[y - 1, ] == "."), fill)
      todo[y - 1, prev_row] <- TRUE
    }
  }
  mm
}

system_time(mmf <- flood_fill(mm))
mmf
sum(mmf == "#") + sum(mmf == ".")

apply(mmf, 1, paste0, collapse = "") %>%
  writeLines("out_18.txt")

# shapes------------
# Incorrect due to thickness of boundary
ls <- st_linestring(verts)
plot(ls, axes = T)
st_polygon(list(ls)) %>% st_area()
st_length(ls)

plot(ls)
pg <- st_sfc(st_polygon(list(ls))) %>%
  st_set_crs(32615)
tb

bf <- st_buffer(ls, dist = 0.5, endCapStyle="SQUARE", joinStyle = "MITRE", singleSide = T)
?st_buffer
plot(bf)
st_area(st_polygon(bf))

# Part 2-------------
# 0 means R, 1 means D, 2 means L, and 3 means U.
lu <- c("R", "D", "L", "U")

tb <- tibble(dir = lu[as.integer(str_sub(txt, -2, -2)) + 1],
             len = strtoi(paste0("0x", str_extract(txt, "(?<=\\#).{5}"))))
tb

move <- list("R" = c(0, 1), "L" = c(0, -1), "U" = c(-1, 0), "D" = c(1, 0))
d_vec <- tb$dir
len_vec <- tb$len

curr <- c(1, 1)
verts <- matrix(NA, length(d_vec), 2)
for (i in seq_along(d_vec)){
  verts[i, ] <- curr
  end <- curr + len_vec[i] * move[[d_vec[i]]]
#  mm[curr[1]:end[1], curr[2]:end[2]] <- "#"
  curr <- end
}
verts <- rbind(verts, verts[1, ])

ls <- st_linestring(verts)
plot(ls, axes = T)
st_polygon(list(ls)) %>% st_area()
st_length(ls)

plot(ls)
pg <- st_sfc(st_polygon(list(ls))) %>%
  st_set_crs(32615)
