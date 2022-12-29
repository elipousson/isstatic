#' What is the orientation of a numeric aspect ratio?
#'
#' @param x A numeric vector with an aspect ratio or a data.frame with width and
#'   height column (names matching the cols parameter).
#' @param tolerance Positive numeric value above or below 1 used to determine if
#'   an aspect ratio is square, landscape, or portrait.
#' @param cols Name of width and height column if x is a data.frame object.
#' @noRd
has_orientation <- function(x, tolerance = 0.1, cols = c("width", "height")) {
  tolerance <- abs(tolerance)

  if (is.data.frame(x)) {
    return(
      has_orientation(
        as.numeric(x[, cols[1]]) / as.numeric(x[, cols[2]]),
        tolerance
      )
    )
  }

  if (length(x) > 1) {
    return(map_chr(x, has_orientation, tolerance))
  }

  if (x > (1 + tolerance)) {
    return("landscape")
  }

  if (x < (1 - tolerance)) {
    return("portrait")
  }

  "square"
}


# from staticimports
# https://github.com/wch/staticimports/blob/main/inst/staticexports/purrr.R
#' @noRd
map_chr <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_character_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_character_)
  }
}
