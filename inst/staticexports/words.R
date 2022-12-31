#' Simple helper for pluralizing words
#'
#' @noRd
plural_words <- function(words,
                         n = 1,
                         suffix = "s",
                         before = "",
                         after = "",
                         replacement = NULL) {
  words <- paste0(before, words, after)

  if (is.null(replacement)) {
    replacement <- paste0(words, suffix)
  }

  if (n > 1) {
    return(replacement)
  }

  words
}
