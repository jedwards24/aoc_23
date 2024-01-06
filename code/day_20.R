library(tidyverse)
library(edwards)
library(bench)
source("functions.R")
#source("code/day_19_func.R")
txt <- readLines("data/test_20.txt")
txt <- readLines("data/data_20.txt")

# Parse text
mod_list <- str_split(txt, " -> ")
mods_txt <- map_chr(mod_list, ~.[[1]])
mods <- str_remove(mods_txt, "\\%|\\&")
mod_type <- str_extract(mods_txt, "\\%|\\&") %>%
  set_names(mods) %>%
  replace_na("broadcaster")
mods_to <- map_chr(mod_list, ~.[[2]]) %>%
  str_split(", ") %>%
  map(~match(., mods))

ff_status <- rep(FALSE, length(mods))

connects_from <- vector("list", length(mods))
for (i in seq_along(mods)){
  connects_from[[i]] <- which(map_lgl(mods_to, ~any(. == i)))
}

conj_status <- map(connects_from, ~rep(FALSE, length(.))) %>%
  map2(.y = connects_from, ~set_names(.x, .y))

push_button <- function(ff_status, conj_status, mods, mods_to, mod_type) {
  todo <- mods_to[[which(mods == "broadcaster")]]
  pulses <- rep(FALSE, length(todo))
  prev <- rep(which(mods == "broadcaster"), length(todo))
  record <- c("Button -low-> broadcaster",
              paste("broadcaster -low->", mods[todo]))
  count <- rep(FALSE, length(pulses) + 1)
  while(length(todo) > 0){
      mi <- todo[1]
      pi <- pulses[1]
      forward <- mods_to[[mi]]
      previ <- prev[1]
      todo <- todo[-1]
      prev <- prev[-1]
      pulses <- pulses[-1]
      if (is.na(mi)) next()
      if (mod_type[mi] == "%" && !pi){ # flip-flop
        ff_status[mi] <- !ff_status[mi]
        todo <- c(todo, forward)
        new_pulses <- rep(ff_status[mi], length(forward))
        pulses <- c(pulses, new_pulses)
        prev <- c(prev, rep(mi, length(forward)))
        count <- c(count, new_pulses)
        record <- c(record,
                    paste(mods[mi],
                          if (ff_status[mi])  "-high->" else "-low->",
                    mods[forward]))
      }
      if (mod_type[mi] == "&"){ # conjunction
        conj <- conj_status[[mi]]
        conj_status[[mi]][which(names(conj) == previ)] <- pi
        todo <- c(todo, forward)
        prev <- c(prev, rep(mi, length(forward)))
        new_pulses <- rep(!all(conj_status[[mi]]), length(forward))
        pulses <- c(pulses, new_pulses)
        count <- c(count, new_pulses)
        record <- c(record,
                    paste(mods[mi],
                          if (!all(conj_status[[mi]]))  "-high->" else "-low->",
                          mods[forward]))

      }
#      if (length(count) == 4) browser()
#      cat(count)
  }
  list(ff_status = ff_status, conj_status = conj_status, pulse_count = count, record = record)
}
push_button(ff_status, conj_status, mods, mods_to, mod_type)
debugonce(push_button)


cur_ff <- ff_status
cur_conj <- conj_status
pulse_history <- vector("list", 1000)
for (i in 1: 1000){
  res <- push_button2(cur_ff, cur_conj, mods, mods_to, mod_type)
  cur_ff <- res$ff_status
  cur_conj <- res$conj_status
  pulse_history[[i]] <- res$pulse_count
}
i
res
ph <- unlist(pulse_history)
sum(ph) * sum(ph == FALSE) # answer
length(ph)
any(str_detect(res$record, "-low-> NA"))

# part 2------------

# A slightly simplified version with different outputs
push_button2 <- function(ff_status, conj_status, mods, mods_to, mod_type) {
  done <- FALSE
  todo <- mods_to[[which(mods == "broadcaster")]]
  pulses <- rep(FALSE, length(todo))
  prev <- rep(which(mods == "broadcaster"), length(todo))
  while(length(todo) > 0){
    mi <- todo[1]
    pi <- pulses[1]
    forward <- mods_to[[mi]]
    previ <- prev[1]
    todo <- todo[-1]
    prev <- prev[-1]
    pulses <- pulses[-1]
    if (is.na(mi)) next()
    # if (is.na(mi)){
    #   if (!pi){
    #     done <- TRUE
    #     break()
    #   }
    #   next()
    # }
    if (mod_type[mi] == "%" && !pi){ # flip-flop
      ff_status[mi] <- !ff_status[mi]
      todo <- c(todo, forward)
      new_pulses <- rep(ff_status[mi], length(forward))
      pulses <- c(pulses, new_pulses)
      prev <- c(prev, rep(mi, length(forward)))
    }
    if (mod_type[mi] == "&"){ # conjunction
      conj <- conj_status[[mi]]
      conj_status[[mi]][which(names(conj) == previ)] <- pi
      todo <- c(todo, forward)
      prev <- c(prev, rep(mi, length(forward)))
      new_pulses <- rep(!all(conj_status[[mi]]), length(forward))
      pulses <- c(pulses, new_pulses)
    }
  }
  list(ff_status = ff_status, conj_status = conj_status)
}

cur_ff <- ff_status
cur_conj <- conj_status
cnt <- 0
res <- list(done = FALSE)
while(!res$done){
  res <- push_button2(cur_ff, cur_conj, mods, mods_to, mod_type)
  cur_ff <- res$ff_status
  cur_conj <- res$conj_status
  cnt <- cnt + 1
  if (cnt %% 10000 == 0) cat(cnt, "\n")
}

bench::mark(push_button(cur_ff, cur_conj, mods, mods_to, mod_type),
            push_button2(cur_ff, cur_conj, mods, mods_to, mod_type), check = F)

#----------
# rg connects from 4 conjunctions
# Each of these connect from a number of FFs
# For a conjunction to send a low pulse it must remember a high pulse from all inputs
# Look for patterns in the groups of FFs that feed into the 4 conjs
which(mods == "rg") #46
x <- connects_from[[46]]
x2 <- connects_from[x] %>% unlist
mods_txt[x]
mods_txt[x2]
connects_from[x2] %>%
  map(~mods_txt[.])
roots <- connects_from[x2]
connects_from[[14]]
mods_txt[c(21, 37)]

res$conj_status[x]
res$conj_status[46]

# Run nn pushes. Collects all results
cur_ff <- ff_status
cur_conj <- conj_status
nn <- 2e4
res_list <- vector("list", nn)
for (i in 1:nn){
  res_list[[i]] <- res <- push_button(cur_ff, cur_conj, mods, mods_to, mod_type)
  cur_ff <- res$ff_status
  cur_conj <- res$conj_status
}

# Lengths of consecutive blocks of 1s and 0s in a numeric vector
len_blocks <- function(x) {
  y <- as.character(x) %>%
    str_flatten()
  zero <- str_extract_all(y, "0+")[[1]] %>%
    str_length
  ones <- str_extract_all(y, "1+")[[1]] %>%
    str_length
  list(zero = zero, one = ones)
}

# SOLUTION METHOD
# Look for low pulses from the four key conjunctions
# Looking at conjunction memory is not right as it may be all high then get reset during a cycle
mods[x2]
mods[x]
st <- paste(mods[x2], "-low->", mods[x])
rr <- map(res_list, ~.[["record"]])
for (sti in st){
  map_lgl(rr, ~any(str_detect(., sti))) %>%
    which() %>%
    cat("\n")
}

3767 * 3779 * 4057 * 3889 # Answer 224602953547789
