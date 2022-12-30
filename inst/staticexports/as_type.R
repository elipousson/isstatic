#' as.integer with option to suppress warnings for NA coercion
#'
#' @inheritParams base::as.integer
#' @noRd
as_integer <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    return(suppressWarnings(as.integer(x)))
  }

  as.integer(x)
}

#' as.roman with option to suppress warnings for NA coercion
#'
#' @inheritParams utils::as.roman
#' @noRd
as_roman <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    return(suppressWarnings(as.roman(x)))
  }

  as.roman(x)
}
