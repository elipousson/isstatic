# `R/is_any.R` is imported from `inst/staticexports/is_any.R`. 
# Please edit that file instead.

#' Do any items in a list or vector return TRUE from a predicate function?
#'
#' @param x A list or vector passed to [vapply()].
#' @param FUN Function passed to FUN parameter of [vapply()].
#' @export
is_any <- function(x, FUN) {
  any(vapply(x, FUN, FUN.VALUE = TRUE))
}

#' Is any item in a list or vector a NULL value?
#'
#' @param x A list or vector to check.
#' @export
is_any_null <- function(x) {
  is_any(x, is.null)
}

#' Is any item in a list or vector a NA value?
#'
#' @param x A list or vector to check.
#' @export
is_any_na <- function(x) {
  is_any(x, is.na)
}
