# `R/is_any.R` is imported from `inst/staticexports/is_any.R`. 
# Please edit that file instead.

#' Do any items in a list or vector return `TRUE` from a predicate function?
#'
#' @param x A list or vector passed to [vapply()].
#' @inheritParams base::vapply
#' @inheritDotParams base::vapply -X
#' @returns `TRUE` if FUN returns `TRUE` for any element of x or `FALSE` if all
#'   elements return `FALSE`.
#' @seealso [isstatic::is_all()]
#' @export
is_any <- function(x, FUN, ...) {
  any(vapply(x, FUN, FUN.VALUE = TRUE, ...))
}

#' - [is_any_null()]: Is any item in a list or vector a `NULL` value?
#'
#' @name is_any_null
#' @rdname is_any
#' @export
is_any_null <- function(x) {
  is_any(x, is.null)
}

#' - [is_any_na()]: Is any item in a list or vector a `NA` value?
#'
#' @name is_any_na
#' @rdname is_any
#' @export
is_any_na <- function(x) {
  is_any(x, is.na)
}
