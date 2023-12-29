# Part 1 method--------------

eval_rule <- function(x, conds, outcomes) {
  eq <- str_extract(conds, "<|>")
  let <- str_sub(conds, 1, 1)
  ind <- c(x = 1, m = 2, a = 3, s = 4)[let]
  num <- as.numeric(str_extract(conds, "\\d+"))
  for (i in seq_along(conds)){
    out_pair <- outcomes[[i]]
    cc <- call(eq[i], x[ind[i]], num[i])
    res <- eval(cc)
    out <- if (res) out_pair[[1]] else out_pair[[2]]
    if (is.logical(out)) return(out)
    if (out != "next_cond") return(eval_rule(x, cond_list[[out]], out_list[[out]]))
  }
  stop("No return: out is ", out, call. = FALSE)
}


# x is single outcome (string)
process_outcome <- function(x) {
  if (x == "A") return(TRUE)
  if (x == "R") return(FALSE)
  if (str_detect(x, "<|>")) return("next_cond")
  x
}

# Part 2 paths method----------
# Paths for each rule
# A path is a vector of conditions plus one outcome
rule_paths <- function(conds, outcomes) {
  for (i in seq_along(conds)){
    curt <- c(conds[i], outcomes[[i]][1])
    curf <- c(cond_rev(conds[i]), outcomes[[i]][2])
    if (i == 1){
      paths <- list(curt, curf)
      next()
    }
    ind <- map_lgl(paths, ~tail(., 1) == conds[i])
    p_false <- p_true <- paths[ind]
    for (j in length(p_true)){
      p_true[[j]] <- path_append(p_true[[j]], curt)
      p_false[[j]] <- path_append(p_false[[j]], curf)
    }
    paths[ind] <- NULL
    paths <- c(paths, p_true, p_false)
  }
  paths
}

# x is a chr vector (path)
# Remove last element then append y
path_append <- function(x, y) {
  c(x[-length(x)], y)
}

# Returns string of the compliment of a condition
# e.g. x = "a>56" returns "a<=56"
cond_rev <- function(x) {
  if (str_detect(x, "<")) return(str_replace(x, "<", ">="))
  str_replace(x, ">", "<=")
}

# Return chr vec of last elements of paths. `x` is a list of paths
get_ends <- function(x) {
  map_chr(x, ~tail(., 1))
}

# x,y are lists of paths
# return list with paths in y appended appropriately onto paths in x
# names of y is needed as args
merge_rule_paths <- function(x, y, nm_y) {
  ind <- which(get_ends(x) == nm_y)
  stopifnot(length(ind) == 1)
  p_old <- x[[ind]]
  p_new <- map(y, ~c(p_old[-length(p_old)], .))
  x[ind] <- NULL
  c(x, p_new)
}

# Total combinations that meet conditions in path x
# x is just vector of conditions with no outcome
path_combs <- function(x, lo = 1, hi = 4000) {
  ltr <- c("x", "m", "a", "s")
  out <- numeric(4)
  for (i in seq_along(ltr)){
    xi <- x[str_sub(x, 1, 1) == ltr[i]]
    if (length(xi) > 0){
      out[i] <- map(xi, ~cond_to_num(., lo, hi)) %>%
        intersect_all() %>%
        length()
    }else{
      out[i] <- hi - lo + 1
    }
  }
  prod(out)
}

# x is string such as "a>=2000".
# returns part of lo:hi that meets this condition
cond_to_num <- function(x, lo = 1, hi = 4000) {
  d <- as.integer(str_extract(x, "\\d+"))
  eq <- str_extract(x, "(>|=|<){1,2}")
  if (eq == ">=") return(d:hi)
  if (eq == ">") return((d + 1):hi)
  if (eq == "<=") return(lo:d)
  if (eq == "<") return(lo:(d - 1))
}

# Function Factory ----------
# This did not work and was hard to debug
# I think the flaw comes from the recursion where the called function
# might not exist when I create the the other function, or might exist in a
# different environments.
if (F){
  factory <- function(conds, outcomes) {
    lu <- c(x = 1, m = 2, a = 3, s = 4)
    eq <- str_extract(conds, "<|>")
    let <- str_sub(conds, 1, 1)
    ind <- lu[let]
    num <- as.numeric(str_extract(conds, "\\d+"))
    out_list <- map(outcomes, ~str_split_1(., ",")) %>%
      map(~map(., process_outcome))
    function(x) {
      for (i in seq_along(conds)){
        out_pair <- out_list[[i]]
        cc <- call(eq[i], x[ind[i]], num[i])
        res <- eval(cc)
        out <- if (res) out_pair[[1]] else out_pair[[2]]
        if (is.logical(out)) return(out)
        if (is.function(out)) return(out(x))
      }
      stop("No return: out is ", out, call. = FALSE)
    }
  }

  # single comndition
  factory1 <- function(conds, outcomes) {
    eq <- str_extract(conds, "<|>")
    let <- str_sub(conds, 1, 1)
    num <- as.numeric(str_extract(conds, "\\d+"))
    function(x, m, a, s) {
      if (eval(call(eq, as.symbol(let), num))) outcomes[1] else outcomes[2]
    }
  }

  # test function
  # x is number, cond is chr
  f <- function(x, cond) {
    eq <- str_extract(cond, "<|>")
    eval(call(eq, x, as.numeric(str_remove(cond, eq))))
  }
}
