# Day 1
# Extracting digits from text
library(tidyverse)
txt <- readLines("data/data_1.txt")


calib_value <- function(x) {
  digs <- str_split_1(x, "") %>%
    str_subset("\\d")
  ends <- c(digs[1], digs[length(digs)])
  as.numeric(str_flatten(ends))
}

vals <- map_int(txt, calib_value)
vals
sum(vals)

# Alternative (more direct) method
first <- str_extract(txt, "\\d{1}")
last <- str_extract(txt, "\\d{1}(?!.*\\d)")
vals1 <- as.integer(paste0(first, last))
identical(vals, vals1)

# part 2 ----------
dig_text <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
dig_pat <- str_c(c(dig_text, "\\d"), collapse = "|")
dig_text_rev <- stringi::stri_reverse(dig_text)
dig_pat_rev <-  dig_text_rev %>%
  {str_c(c(., "\\d"), collapse = "|")}

# length 1 input
convert_digits <- function(x, text = dig_text) {
  if (x %in% text) return(match(x, text))
  as.integer(x)
}

calib_value2 <- function(x) {
  first <- str_extract(x, dig_pat)
  last <- str_extract(stringi::stri_reverse(x), dig_pat_rev)
  ends <- c(convert_digits(first), convert_digits(last, dig_text_rev))
  as.numeric(str_flatten(ends))
}

vals2 <- map_int(txt, calib_value2)
sum(vals2)

tt <- tibble(txt, vals, vals2, vals3)
tt
