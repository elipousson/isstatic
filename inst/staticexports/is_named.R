#' Is this object a named list or character vector?
#'
#' @param x A data frame or another named object.
#' @seealso [rlang::is_named()]
#' @noRd
is_named <- function(x) {
  !is.null(names(x)) && is_none_in("", names(x))
}


#' Does this object have all of the provided names?
#'
#' @rdname is_named
#' @name has_all_names
#' @param name Element name(s) to check.
#' @noRd
has_all_names <- function(x, name) {
  if (anyNA(c(x, name))) {
    return(FALSE)
  }

  all(utils::hasName(x, name))
}


#' Does this object have any of the provided names?
#'
#' @rdname is_named
#' @name has_any_names
#' @noRd
has_any_names <- function(x, name) {
  if (anyNA(c(x, name))) {
    return(FALSE)
  }

  any(utils::hasName(x, name))
}
