#' Apply a function to each element of a vector.
#'
#' @author Winston Chang \email{winston@stdout.org}
#'
#' @source [purr-like functions](https://github.com/wch/staticimports/blob/main/inst/staticexports/purrr.R) in [staticimports](https://wch.github.io/staticimports/) package
#
#' @noRd
map_chr <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_character_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_character_)
  }
}

#' Convert to a common sentence case
#'
#' @author Joachim Schork \email{info@joachimschork.com}
#'
#' @source [Statistics Globe](https://statisticsglobe.com/r-capitalize-first-letter-of-character-string-containing-multiple-words)
#'
#' @noRd
tosentence <- function(x) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
}

#' Combine multiple words into a single string
#'
#' @author Yihui Xie \email{xie@yihui.name}
#'   ([ORCID](https://orcid.org/0000-0003-0645-5666))
#'
#' @source Adapted from [knitr::combine_words()] in the
#'   [knitr](https://yihui.org/knitr/) package.
#'
#' @inheritParams knitr::combine_words
#' @noRd
combine_words <- function(words,
                          sep = ", ",
                          and = " and ",
                          before = "",
                          after = before,
                          oxford_comma = TRUE) {
  n <- length(words)

  if (n == 0) {
    return(words)
  }

  words <- paste0(before, words, after)

  if (n == 1) {
    return(rs(words))
  }

  if (n == 2) {
    return(rs(paste(words, collapse = if (is_blank(and)) sep else and)))
  }

  if (oxford_comma && grepl("^ ", and) && grepl(" $", sep)) {
    and <- gsub("^ ", "", and)
  }

  words[n] <- paste0(and, words[n])

  if (!oxford_comma) {
    words[n - 1] <- paste0(words[n - 1:0], collapse = "")
    words <- words[-n]
  }

  paste(words, collapse = sep)
}
