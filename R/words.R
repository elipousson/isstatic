#' @noRd
plural_words <- function(words,
                         n = 1,
                         suffix = "s",
                         before = "",
                         after = "",
                         replacement = NULL) {
  words = paste0(before, words, after)

  if (n > 1) {
    return(replacement %||% paste0(words, suffix))
  }

  words
}
