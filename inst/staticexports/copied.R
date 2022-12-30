# Copied from staticimports
# https://github.com/wch/staticimports/blob/main/inst/staticexports/operators.R
#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# Copied from staticimports
# https://github.com/wch/staticimports/blob/main/inst/staticexports/purrr.R
#' @noRd
map_chr <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_character_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_character_)
  }
}

#' @noRd
tosentence <- function(x) {
  # Copied from https://statisticsglobe.com/r-capitalize-first-letter-of-character-string-containing-multiple-words
  gsub(
    "(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
    x,
    perl = TRUE
  )
}

#' Copied from [knitr::combine_words()]
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
