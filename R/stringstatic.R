# `R/stringstatic.R` is imported from `inst/staticexports/stringstatic.R`. 
# Please edit that file instead.

# ======================================================================
# Imported from pkg:stringstatic on 2023-03-15
# ======================================================================

#' Join multiple strings into a single string
#'
#' Dependency-free drop-in alternative for `stringr::str_c()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param ... One or more character vectors.
#'   Zero length arguments are removed.
#'   Short arguments are recycled to the length of the longest.
#'
#'   Like most other R functions, missing values are "infectious":
#'   whenever a missing value is combined with another string
#'   the result will always be missing.
#'   Use `str_replace_na()` to convert `NA` to "NA"
#'
#' @param sep String to insert between input vectors.
#'
#' @param collapse
#'   Optional string used to combine input vectors into single string.
#'
#' @return If `collapse = NULL` (the default) a character vector
#'   with length equal to the longest input string.
#'   If collapse is non-`NULL`, a character vector of length 1.
#' @export
str_c <- function(..., sep = "", collapse = NULL) {
  stopifnot(
    "`sep` must be a single string, not a character vector." = length(sep) == 1,
    "`collapse` must be a single string or `NULL`, not a character vector." =
      length(collapse) == 1 || is.null(collapse)
  )

  strings <- Filter(function(x) !is.null(x), list(...))

  if (length(strings) == 0 || any(lengths(strings) == 0)) {
    if (length(collapse) == 0) return(character(0))
    return("")
  }

  max_length <- max(lengths(strings))

  result <- lapply(strings, rep_len, length.out = max_length)
  result <- do.call(cbind, result)
  result <- apply(result, 1, paste, collapse = sep)
  result <- paste(result, collapse = collapse)

  result
}

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
  if (length(string) == 0 || length(pattern) == 0) return(logical(0))

  is_fixed <- inherits(pattern, "stringr_fixed")

  indices <- Vectorize(grep, c("pattern", "x"), USE.NAMES = FALSE)(
    pattern,
    x = string,
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
  if (length(string) == 0 || length(pattern) == 0) return(character(0))

  is_fixed <- inherits(pattern, "stringr_fixed")

  result <- Map(
    function(string, pattern) {
      if (is.na(string) || is.na(pattern)) return(NA_character_)

      regmatches(
        x = string,
        m = regexpr(
          pattern = pattern, text = string, perl = !is_fixed, fixed = is_fixed
        )
      )
    },
    string, pattern, USE.NAMES = FALSE
  )

  result[lengths(result) == 0] <- NA_character_
  unlist(result)
}

#' Compute the length of a string
#'
#' Dependency-free drop-in alternative for `stringr::str_length()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @return A numeric vector the same length as string.
#' @export
str_length <- function(string) {
  nchar(as.character(string), type = "chars", keepNA = TRUE)
}

#' Duplicate and concatenate strings within a character vector
#'
#' Dependency-free drop-in alternative for `stringr::str_pad()`.
#'
#' @author Eli Pousson \email{eli.pousson@gmail.com}
#'   ([ORCID](https://orcid.org/0000-0001-8280-1706))
#'
#'   Alexander Rossell Hayes \email{alexander@rossellhayes.com}
#'   ([ORCID](https://orcid.org/0000-0001-9412-0457))
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#' @param width Minimum width of padded strings.
#' @param side Side on which padding character is added (left, right or both).
#' @param pad Single padding character (default is a space).
#' @param use_width If `FALSE`,
#'   use the length of the string instead of the width;
#'   see [str_width()]/[str_length()] for the difference.
#'
#' @return A character vector.
#' @export
str_pad <- function(
    string, width, side = c("left", "right", "both"), pad = " ", use_width = TRUE
) {
  if (!is.numeric(width)) {
    return(string[NA])
  }

  if (any(nchar(pad, type = "width") != 1)) {
    stop("each string in `pad` should consist of code points of total width 1")
  }

  side <- match.arg(side)

  nchar_type <- if (isTRUE(use_width)) "width" else "chars"
  string_width <- nchar(string, nchar_type)
  pad_width <- width - string_width
  pad_width[pad_width < 0] <- 0

  switch(
    side,
    "left" = paste0(strrep(pad, pad_width), string),
    "right" = paste0(string, strrep(pad, pad_width)),
    "both" = paste0(
      strrep(pad, floor(pad_width / 2)),
      string,
      strrep(pad, ceiling(pad_width / 2))
    )
  )
}

#' Remove matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_remove()`.
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
#' @return A character vector.
#' @export
str_remove <- function(string, pattern) {
  if (length(string) == 0 || length(pattern) == 0) return(character(0))
  is_fixed <- inherits(pattern, "stringr_fixed")
  Vectorize(sub, c("pattern", "x"), USE.NAMES = FALSE)(
    pattern, replacement = "", x = string, perl = !is_fixed, fixed = is_fixed
  )
}

#' Replace matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_replace()`.
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
#' @param replacement A character vector of replacements.
#'   Should be either length one, or the same length as `string` or `pattern`.
#'   References of the form `\1`, `\2`, etc. will be replaced with the contents
#'   of the respective matched group (created by `()`).
#'
#'   To replace the complete string with `NA`,
#'   use `replacement = NA_character_`.
#'
#'   Using a function for `replacement` is not yet supported.
#'
#' @return A character vector.
#' @export
str_replace <- function(string, pattern, replacement) {
  if (length(string) == 0 || length(pattern) == 0 || length(replacement) == 0) {
    return(character(0))
  }

  is_fixed <- inherits(pattern, "stringr_fixed")

  Vectorize(sub, c("pattern", "replacement", "x"), USE.NAMES = FALSE)(
    pattern, replacement, x = string, perl = !is_fixed, fixed = is_fixed
  )
}

#' Replace matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_replace_all()`.
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
#' @param replacement A character vector of replacements.
#'   Should be either length one, or the same length as `string` or `pattern`.
#'   References of the form `\1`, `\2`, etc. will be replaced with the contents
#'   of the respective matched group (created by `()`).
#'
#'   To perform multiple replacements in each element of `string`,
#'   pass a named vector `(c(pattern1 = replacement1))` to `str_replace_all()`.
#'
#'   To replace the complete string with `NA`,
#'   use `replacement = NA_character_`.
#'
#'   Using a function for `replacement` is not yet supported.
#'
#' @return A character vector.
#' @export
str_replace_all <- function(string, pattern, replacement) {
  is_fixed <- inherits(pattern, "stringr_fixed")

  if (!is.null(names(pattern))) {
    for (i in seq_along(pattern)) {
      string <- gsub(
        pattern = names(pattern)[[i]],
        replacement = pattern[[i]],
        x = string,
        perl = !is_fixed,
        fixed = is_fixed
      )
    }
    return(string)
  }

  if (length(string) == 0 || length(pattern) == 0 || length(replacement) == 0) {
    return(character(0))
  }

  Vectorize(gsub, c("pattern", "replacement", "x"), USE.NAMES = FALSE)(
    pattern, replacement, x = string, perl = !is_fixed, fixed = is_fixed
  )
}

#' Compute the width of a string
#'
#' Dependency-free drop-in alternative for `stringr::str_width()`.
#' Results for non-ASCII characters may be inaccurate in R < 4.0.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @return A numeric vector the same length as string.
#' @export
str_width <- function(string) {
  nchar(as.character(string), type = "width", keepNA = TRUE)
}
