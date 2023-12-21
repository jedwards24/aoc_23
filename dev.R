# `blocks` is a record split by dots.
dp_blocks3 <- function(blocks, nn) {
  if (length(nn) == 0){
    if (str_detect(str_flatten(blocks), "\\#")){
      return(0)
    }
    return(1)
  }
  if (length(blocks) == 0) return(0)
  total <- 0
  if (length(blocks) == 1) return(count_arrange3(str_split_1(blocks[1], ""), nn))
  if (!str_detect(blocks[1], "\\#")){
    total <- total + dp_blocks(blocks[-1], nn)
  }
  for (i in 1 : length(nn)){
    nset <- nn[1:i]
    if (str_length(blocks[1]) < sum(nset) + length(nset) - 1) break()
    total <- total +
      dp_blocks(blocks[-1], nn[-c(1:i)]) *
      count_arrange3(str_split_1(blocks[1], ""), nset)
  }
  total
}

# rr is vector of single characters
count_arrange3 <- function(rr, nn) {
#  if (any(is.na(rr))) browser()
#  if (length(nn) == 1) browser()
  if (length(nn) == 0){
    if (any(rr == "#")){
      return(0)
    }
    return(1)
  }
  if (length(nn) == 1 && length(rr) == nn) return(1)
  if (length(rr) < sum(nn) + length(nn) - 1) return(0)
  if (!any(rr == "#")){
    return(choose(length(rr) + 1 - sum(nn), length(nn)))
  }
  if (rr[nn[1] + 1] != "#"){ # check is valid
    if (rr[1] == "#"){
      return(count_arrange3(rr[-c(1:(nn[1] + 1))], nn[-1]))
    }
    return(count_arrange3(rr[-c(1:(nn[1] + 1))], nn[-1]) +
             count_arrange3(rr[-1], nn))
  }
  if (rr[1] == "#") return(0)
  return(count_arrange3(rr[-1], nn))
}

mark(str_length("hhhhh"), length(1:5))
x <- "....#..."
x2 <- str_split_1(x, "")
mark(str_detect(x, "\\#"), any(x2 == "#"))
debugonce(count_arrange3)

nn <- 6
recs2 <- str_remove(recs, "\\.+$") %>%
  str_remove("^\\.+")
recs2[nn]
#system_time(sol <- map2_int(recs2[nn], nums[nn], ~count_arrange(.x, .y), .progress = TRUE))
#system_time(sol <- map2_int(recs2[nn], nums[nn], ~dp_blocks(str_split_1(.x, "\\.+"), .y)))
system_time(sol2 <- map2_int(recs2[nn], nums[nn], ~dp_blocks3(str_split_1(.x, "\\.+"), .y)))
