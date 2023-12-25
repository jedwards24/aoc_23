# Get number of valid arrangements for record `rec` and
# `num` - groups of damaged springs `
n_arrange <- function(rec, num) {
  nhash <- str_count(rec, "#")
  rr <- str_split_1(rec, "")
  qm <- str_which(rr, "\\?")
  cm <- combn(length(qm), sum(num) - nhash, simplify = FALSE)
  hash_ind <- map(cm, ~qm[.])
  rec_to_test <- map_chr(hash_ind, ~replace_qms(rr, .))
  sum(map_lgl(rec_to_test, test_arrange, num = num))
}

test_arrange <- function(rec, num) {
  lens <- unlist(str_match_all(rec, "\\#+")) %>%
    str_length()
  if (length(lens) != length(num)) return (FALSE)
  all(lens == num)
}

# x is split record. Returns a collapsed string
replace_qms <- function(x, hash_ind) {
  x[hash_ind] <- "#"
  str_flatten(str_replace(x, "\\?", "."))
}

# rr is record, nn is lengths of contiguous blocks (num)
count_arrange <- function(rr, nn) {
  rr <- str_remove(rr, "^\\.+")
  if (length(nn) == 0){
    if (str_detect(rr, "\\#")){
      return(0)
    }
    return(1)
  }
  if (str_length(rr) < sum(nn) + length(nn) - 1) return(0L)
  valid_now <- !(str_detect(str_sub(rr, 1, nn[1]), "\\.") ||
                   str_sub(rr, nn[1] + 1, nn[1] + 1) == "#")
  if (str_sub(rr, 1, 1) == "#"){
    if (valid_now){
      return(count_arrange(str_sub(rr, nn[1] + 2), nn[-1]))
    }
    return(0)
  }
  if (!valid_now){
    return(count_arrange(str_sub(rr, 2), nn))
  }
  return(count_arrange(str_sub(rr, nn[1] + 2), nn[-1]) +
           count_arrange(str_sub(rr, 2), nn))
}

# rr is record, nn is lengths of contiguous blocks (num)
# Similar to count_arrange() but with no dots in rr input
count_arrange2 <- function(rr, nn) {
  if (length(nn) == 0){
    if (str_detect(rr, "\\#")){
      return(0)
    }
    return(1)
  }
  if (length(nn) == 1 && str_length(rr) == nn) return(1)
  if (str_length(rr) < sum(nn) + length(nn) - 1) return(0)
  if (!str_detect(rr, "\\#")){
    return(choose(str_length(rr) + 1 - sum(nn), length(nn)))
  }
  if (str_sub(rr, nn[1] + 1, nn[1] + 1) != "#"){ # check is valid
    if (str_sub(rr, 1, 1) == "#"){
      return(count_arrange2(str_sub(rr, nn[1] + 2), nn[-1]))
    }
    return(count_arrange2(str_sub(rr, nn[1] + 2), nn[-1]) +
             count_arrange2(str_sub(rr, 2), nn))
  }
  if (str_sub(rr, 1, 1) == "#") return(0)
  return(count_arrange2(str_sub(rr, 2), nn))
}

# `blocks` is a record split by dots.
dp_blocks <- function(blocks, nn) {
  if (length(nn) == 0){
    if (str_detect(str_flatten(blocks), "\\#")){
      return(0)
    }
    return(1)
  }
  if (length(blocks) == 0) return(0)
  total <- 0
  if (length(blocks) == 1) return(count_arrange2(blocks[1], nn))
  if (!str_detect(blocks[1], "\\#")){
    total <- total + dp_blocks(blocks[-1], nn)
  }
  for (i in 1 : length(nn)){
    nset <- nn[1:i]
    if (str_length(blocks[1]) < sum(nset) + length(nset) - 1) break()
    total <- total +
      dp_blocks(blocks[-1], nn[-c(1:i)]) *
      count_arrange2(blocks[1], nset)
  }
  total
}

# `blocks` is a record split by dots.
# Calls count_arrange3()
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
    total <- total + dp_blocks3(blocks[-1], nn)
  }
  for (i in 1 : length(nn)){
    nset <- nn[1:i]
    if (str_length(blocks[1]) < sum(nset) + length(nset) - 1) break()
    total <- total +
      dp_blocks3(blocks[-1], nn[-c(1:i)]) *
      count_arrange3(str_split_1(blocks[1], ""), nset)
  }
  total
}

# rr is vector of single characters
count_arrange3 <- function(rr, nn) {
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


# Proper DP approach-------------
# Record counts in a matrix and reuse
count_arrange4 <- function(rr, nn) {
  rr <- str_remove(rr, "^\\.+") %>%
    str_remove("\\.+$") %>%
    str_split_1("")
  vals <- matrix(NA_real_, nrow = length(nn), ncol = length(rr))
  lenn <- length(nn)
  lenr <- length(rr)
  for (i in seq_along(nn)){
    for (j in seq_along(rr)){
      vals[i, j] <- value_state(rr, nn, i, j, vals)
    }
  }
  vals[lenn, lenr]
}

# Called from count_arrange4
#
value_state <- function(rr, nn, i, j, vals) {
  lenn <- length(nn)
  lenr <- length(rr)
  if (j < sum(tail(nn, i)) + i - 1) return(0)
  if (i == 1){
    if (j < nn[lenn]) return(0)
    rcur <- tail(rr, j)
    if (j == nn[lenn]) return(!any(rcur == "."))
    option_now <- !any(rcur[1:nn[lenn]] == ".") && !any(rcur[(nn[lenn] + 1):length(rcur)] == "#")
    if(option_now){
      if (rcur[1] == "#") return(1)
      return(vals[i, j - 1] + 1)
    }
    if (rcur[1] == "#") return(0) else return(vals[i, j - 1])
  }
  rcur <- tail(rr, j)
  ncur <- tail(nn, i)
  if(!any(rcur[1:ncur[1]] == ".") && !rcur[ncur[1] + 1] == "#"){
    if (rcur[1] == "#") return(vals[i - 1, j - ncur[1] - 1])
    return(vals[i, j - 1] + vals[i - 1, j - ncur[1] - 1])
  }
  if (rcur[1] == "#") return(0)
  vals[i, j - 1]
}

# `blocks` is a record split by dots.
# Calls count_arrange5()
dp_blocks5 <- function(blocks, nn) {
  if (length(nn) == 0){
    if (str_detect(str_flatten(blocks), "\\#")){
      return(0)
    }
    return(1)
  }
  if (length(blocks) == 0) return(0)
  total <- 0
  if (length(blocks) == 1) return(count_arrange5(blocks[1], nn))
  if (!str_detect(blocks[1], "\\#")){
    total <- total + dp_blocks5(blocks[-1], nn)
  }
  for (i in 1 : length(nn)){
    nset <- nn[1:i]
    if (str_length(blocks[1]) < sum(nset) + length(nset) - 1) break()
    total <- total +
      dp_blocks5(blocks[-1], nn[-c(1:i)]) *
      count_arrange5(blocks[1], nset)
  }
  total
}

# as count_arrange4 but calling value_state2 - no dots in rr
count_arrange5 <- function(rr, nn) {
  rr <- str_split_1(rr, "")
  vals <- matrix(NA_real_, nrow = length(nn), ncol = length(rr))
  lenn <- length(nn)
  lenr <- length(rr)
  for (i in seq_along(nn)){
    for (j in seq_along(rr)){
      vals[i, j] <- value_state2(rr, nn, i, j, vals)
    }
  }
  vals[lenn, lenr]
}

# Called from count_arrange4
value_state2 <- function(rr, nn, i, j, vals) {
  lenn <- length(nn)
  lenr <- length(rr)
  if (j < sum(tail(nn, i)) + i - 1) return(0)
  if (i == 1){
    if (j < nn[lenn]) return(0)
    rcur <- tail(rr, j)
    if (j == nn[lenn]) return(1)
    option_now <- !any(rcur[(nn[lenn] + 1):length(rcur)] == "#")
    if(option_now){
      if (rcur[1] == "#") return(1)
      return(vals[i, j - 1] + 1)
    }
    if (rcur[1] == "#") return(0) else return(vals[i, j - 1])
  }
  rcur <- tail(rr, j)
  ncur <- tail(nn, i)
  if(!rcur[ncur[1] + 1] == "#"){
    if (rcur[1] == "#") return(vals[i - 1, j - ncur[1] - 1])
    return(vals[i, j - 1] + vals[i - 1, j - ncur[1] - 1])
  }
  if (rcur[1] == "#") return(0)
  vals[i, j - 1]
}
