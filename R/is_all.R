# `R/is_all.R` is imported from `inst/staticexports/is_all.R`. 
# Please edit that file instead.

#' Do all items in a list or vector return TRUE from a predicate function?
#'
#' @param x A list or vector passed to [vapply()].
#' @param FUN Function passed to FUN parameter of [vapply()].
#' @export
is_all <- function(x, FUN, ...) {
  all(vapply(x, FUN, FUN.VALUE = TRUE, ...))
}

#' Are all items in a list or vector NULL values?
#'
#' @param x A list or vector to check.
#' @export
is_all_null <- function(x) {
  is_all(x, is.null)
}

#' Are all items in a list or vector NA values?
#'
#' @param x A list or vector to check.
#' @export
is_all_na <- function(x) {
  is_all(x, is.na)
}
