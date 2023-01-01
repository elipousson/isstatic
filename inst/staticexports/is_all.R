#' Do all items in a list or vector return TRUE from a predicate function?
#'
#' @param x A list or vector passed to X parameter of [vapply()].
#' @inheritParams base::vapply
#' @inheritDotParams base::vapply -X
#' @returns `TRUE` if FUN returns `TRUE` for all elements of x or `FALSE` if any
#'   element returns `FALSE`.
#' @seealso [is_any()]
#' @noRd
is_all <- function(x, FUN, ...) {
  all(vapply(x, FUN, FUN.VALUE = TRUE, ...))
}

#' - [is_all_null()]: Are all items in a list or vector `NULL` values?
#'
#' @name is_all_null
#' @rdname is_all
#' @noRd
is_all_null <- function(x) {
  is_all(x, is.null)
}

#' - [is_all_na()]: Are all items in a list or vector NA values?
#'
#' @name is_all_na
#' @rdname is_all
#' @noRd
is_all_na <- function(x) {
  is_all(x, is.na)
}
