# Started with mapping the whole trench and using flood fill but the matrix would be too big for part 2.
# So just found the corners and used sf for the area. The complication was adjusting for the thickness
# of the border. Once I had done this it was a very efficient method
library(tidyverse)
library(edwards)
library(bench)
library(sf)
source("functions.R")
source("code/day_18_func.R")
test <- T
txt <- readLines("data/data_18.txt")
if (test) txt <- readLines("data/test_18.txt")

tb <- tibble(dir = str_sub(txt, 1, 1),
       len = as.numeric(str_extract(txt, "(?<= )\\d+")))
count_nas(tb)
tb

# Estimate size of matrix needed
tb2 <- cum_tbl(tb)
summary(tb2$cv)
summary(tb2$ch)

# Run-------
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

system_time(mmf <- flood_fill(mm))
mmf
sum(mmf == "#") + sum(mmf == ".")

apply(mmf, 1, paste0, collapse = "") %>%
  writeLines("out_18.txt")

# Part 2-------------
# 0 means R, 1 means D, 2 means L, and 3 means U.
tb <- tibble(dir = lu[as.integer(str_sub(txt, -2, -2)) + 1],
             len = strtoi(paste0("0x", str_extract(txt, "(?<=\\#).{5}"))))

# Run data
tb
tail(tb)

# corner starts: 1 for both tests and part 1. 4 for part 2
# Need to look at the first and last moves but that leaves two possible corners.
verts <- dig_verts(tb, 4)
ls <- st_linestring(verts)
plot(ls, axes = T)
st_polygon(list(ls)) %>% st_area() # answer

# Unused---------
st_length(ls)
plot(ls)
pg <- st_sfc(st_polygon(list(ls))) %>%
  st_set_crs(32615)


plot(ls)
pg <- st_sfc(st_polygon(list(ls))) %>%
  st_set_crs(32615)
tb

bf <- st_buffer(ls, dist = 0.5, endCapStyle="SQUARE", joinStyle = "MITRE", singleSide = T)
?st_buffer
plot(bf)
st_area(st_polygon(bf))
