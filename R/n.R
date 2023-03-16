# `R/n.R` is imported from `inst/staticexports/n.R`. 
# Please edit that file instead.

#' Get the n most frequent or least frequent appearing values in a vector
#'
#' This function does not address ties in frequency.
#'
#' @param string A character vector.
#' @param n The number of values to return based on frequency of appearance.
#'   Defaults to `NULL` which returns all unique values from x.
#' @param decreasing Passed to [sort()]
#' @export
str_n_freq <- function(string = NULL, n = NULL, decreasing = TRUE) {
  # https://stackoverflow.com/questions/17374651/find-the-n-most-common-values-in-a-vector
  string <- sort(table(string), decreasing = decreasing)
  if (is.null(n)) {
    n <- length(string)
  }
  static_check_numeric(n)
  names(string)[1:n]
}

