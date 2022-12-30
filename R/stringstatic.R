# `R/stringstatic.R` is imported from `inst/staticexports/stringstatic.R`. 
# Please edit that file instead.

#' Detect the presence or absence of a pattern in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_detect()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @param negate If `TRUE`, return non-matching elements.
#'
#' @return A logical vector.
#' @export
str_detect <- function(string, pattern, negate = FALSE) {
  is_fixed <- inherits(pattern, "fixed")
  ignore.case <- isTRUE(attr(pattern, "options")$case_insensitive)

  if (length(string) == 0 || length(pattern) == 0) {
    return(logical(0))
  }

  indices <- Vectorize(grep, c("pattern", "x"), USE.NAMES = FALSE)(
    pattern,
    x = string,
    ignore.case = ignore.case,
    perl = !is_fixed,
    fixed = is_fixed,
    invert = negate
  )

  result <- as.logical(lengths(indices))
  result[is.na(string)] <- NA
  result
}

#' Extract matching patterns from a string
#'
#' Dependency-free drop-in alternative for `stringr::str_extract()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @return A character matrix.
#'   The first column is the complete match,
#'   followed by one column for each capture group.
#' @export
str_extract <- function(string, pattern) {
  ignore.case <- isTRUE(attr(pattern, "options")$case_insensitive)
  is_fixed <- !ignore.case && inherits(pattern, "fixed")

  if (length(string) == 0 || length(pattern) == 0) {
    return(character(0))
  }

  result <- Map(
    function(string, pattern) {
      if (is.na(string) || is.na(pattern)) {
        return(NA_character_)
      }

      regmatches(
        x = string,
        m = regexpr(
          pattern = pattern,
          text = string,
          ignore.case = ignore.case,
          perl = !is_fixed,
          fixed = is_fixed
        )
      )
    },
    string, pattern,
    USE.NAMES = FALSE
  )

  result[lengths(result) == 0] <- NA_character_
  unlist(result)
}
