#' Compares >=2 objects with R function all.equal()
#' 
#' @param ... >=2 comma-separated object names
#' @return 
#' \code{all_equal_mult} returns TRUE or list of pairwise all.equal() object comparisons
#' @examples
#'  foo <- c(1:10)
#'  bar <- c(1:10)
#'  foz <- c(1:10)
#'  baz <- letters[1:10]
#'
#'  all_equal_mult(foo, bar) # TRUE
#'  all_equal_mult(foo, baz) # results of all.equal(foo, baz) as one-item list
#'  all_equal_mult(foo, bar, foz) # TRUE
#'  all_equal_mult(foo, bar, baz) # list of pairwise all.equal() comparisons among objects
#' @export all_equal_mult
all_equal_mult <- function(...) {
  # more than one object required
  if (length(list(...)) < 2) stop("More than one object required")

  # character vector of object names
  names <- as.character(substitute(list(...)))[-1L]

  # matrix of object name pairs
  pairs <- t(combn(names, 2))  

  # if only two objects, return one item list containing all.equal() for them
  if (nrow(pairs) == 1) return(list(all.equal(get(pairs[1,1]), get(pairs[1,2]))))

  # function: eq.fun()
  # description: applies all.equal() to two quoted names of objects
  # input: two quoted names of objects
  # output: list containing all.equal() comparison and "[obj1] vs. [obj2]"
  # examples:
  #   x <- 1
  #   y <- 1
  #   z <- 2
  #   eq.fun("x", "y") # list(TRUE, "x vs. y")
  #   eq.fun("x", "z") # list("Mean relative difference: 1", "x vs. z")
  eq.fun <- function(x, y) {
    all.eq <- all.equal(get(x, inherits=TRUE), get(y, inherits=TRUE))
    name <- paste0(x, " vs. ", y)
    return(list(all.eq, name))
    }

  # list of eq.fun object comparisons
  out <- vector(mode="list", length=nrow(pairs))

  for (x in 1:nrow(pairs)) {
    eq.list <- eq.fun(pairs[x, 1], pairs[x, 2])
    out[[x]] <- eq.list[[1]]
    names(out)[x] <- eq.list[[2]]
    }

  # return TRUE if all objects equal, comparison list otherwise
  if (mode(unlist(out)) == "logical") {return(TRUE)} else {return(out)}
  }