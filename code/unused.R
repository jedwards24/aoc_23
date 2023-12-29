# Day 12 Decomposition method------------

r <- recs[2]
nn <- nums[[2]]
rr <- str_split_1(r, "\\.+")
nn

# Want all possible  assignments of nn to rr
# 1. All permutations of nn then all splits of each
# 2. All subsets of nn then combine

n <- length(nn)
# all subsets of nn
ss <- map(0:15, ~nn[as.logical(int2bin(., 4))])
ss
subs <- map(ss, sort) %>% unique()
lens <- map_int(subs, ~sum(.) + length(.) - 1)

# ss are sets, x is space available
# returns indices of ss
possible_sets <- function(x, ss) {
  lens <- map_int(ss, ~sum(.) + length(.) - 1)
  which(lens <= x)
}
poss <- map(str_length(rr), ~possible_sets(., subs))
expand_grid(poss)
expand.grid(poss) %>%
  as.matrix()
2 * 7^2

tb <- expand.grid(poss) %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(inds = list(c_across(everything()))) %>%
  select(inds) %>%
  ungroup() %>%
  mutate(sets = map(inds, ~subs[.])) %>%
  mutate(total_set = map(sets, ~sort(unlist(.)))) %>%
  mutate(valid = map_lgl(total_set, ~identical(., nns)))

tb$sets[[3]]
nns <- sort(nn)
valid <- map(tb$sets, ~sort(unlist(.))) %>%
  map_lgl(~identical(., nns))

nn_sets <- tb %>%
  filter(valid) %>%
  pull(sets)

rr


n_arrange_sub <- function(num_list, rr) {
  prod(map2_int(rr, num_list, ~n_arrange(.x, .y)))
}
n_arrange_sub(nn_sets[[1]], rr)
n_arrange(rr[3], nn_sets[[1]][[3]])
map_int(nn_sets, ~n_arrange_sub(., rr))
n_arrange(recs[2], nums[[2]])
#map2_int(recs, nums, ~n_arrange(.x, .y), .progress = TRUE)

n_arrange(rr[3], nn_sets[[1]][[3]]) #??


## Day 19. Tree creation -----------
# Could use data.tree package but try a list first
# Nodes are conditions

i = 1
conds[[i]]
out_list[[i]]
outcomes[i]
rules[i]

out_list[[1]]
i = 1
list(split = pluck(conds, i, 1), out = pluck(out_list, i, 1))

mini_tree <- function(cond, out) {
  list(split = cond,
       out = out)
}

combined_list <- map2(cond_list, out_list, ~map2(.x, .y, mini_tree))
combined_list[[1]][[1]]

# merges by matching splits
merge_tree <- function(x) {
  for (i in length(x):1){
    if (length(x) == 1) break()
    sp <- which(pluck(x, i - 1, "out") == pluck(x, i, "split"))
    x[[i-1]]$out <- list(pluck(x, i-1, "out")[-sp], x[[i]])
    x[[i]] <- NULL
  }
  x
}
merge_tree(combined_list[[1]])

merged_list <- map(combined_list, merge_tree) %>%
  setNames(fnames)
