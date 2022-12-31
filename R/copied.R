# `R/copied.R` is imported from `inst/staticexports/copied.R`. 
# Please edit that file instead.

#' Apply a function to each element of a vector.
#'
#' @author Winston Chang \email{winston@stdout.org}
#'
#' @source [purr-like functions](https://github.com/wch/staticimports/blob/main/inst/staticexports/purrr.R) in [staticimports](https://wch.github.io/staticimports/) package
#
#' @export
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
#' @param x a character vector, or an object that can be coerced to character by
#'   [as.character()].
#' @export
tosentence <- function(x) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
}


#' @inherit xfun::is_blank
#'
#' @author Yihui Xie \email{xie@yihui.name}
#'   ([ORCID](https://orcid.org/0000-0003-0645-5666))
#'
#' @source Adapted from [xfun::is_blank()] in the
#'   [xfun](https://yihui.org/xfun/) package.
#'
#' @examples
#' is_blank("")
#' is_blank("abc")
#' is_blank(c("", "  ", "\n\t"))
#' is_blank(c("", " ", "abc"))
#' @export
is_blank <- function(x) {
  all(grepl("^\\s*$", x))
}


#' Combine multiple words into a single string
#'
#' @author Yihui Xie \email{xie@yihui.name}
#'   ([ORCID](https://orcid.org/0000-0003-0645-5666))
#'
#' @source Adapted from [knitr::combine_words()] in the
#'   [knitr](https://yihui.org/knitr/) package.
#'
#' @inherit knitr::combine_words
#' @returns A character string
#' @export
combine_words <- function(words,
                          sep = ", ",
                          and = " and ",
                          before = "",
                          after = before,
                          oxford_comma = TRUE) {
  n <- length(words)

  rs <- function (x) {
    if (is.null(x))
      x = as.character(x)
    x
  }

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

  rs(paste(words, collapse = sep))
}
