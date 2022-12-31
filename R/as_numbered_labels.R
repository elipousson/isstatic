# `R/as_numbered_labels.R` is imported from `inst/staticexports/as_numbered_labels.R`. 
# Please edit that file instead.

#' Convert a numeric vector to a vector of numbered labels
#'
#' This function allows the creation of numbered labels for a vector using a
#' range of numbering styles.
#'
#' @param x An integer or other vector or a data.frame. An integer vector or
#'   integer column is used as the number that is converted based on the label
#'   style. If x is not an integer or data.frame with an integer column, the
#'   numbering is created based on [seq_along()].
#' @param labels Label style. Options include "arabic", "alph", "Alph", "alpha",
#'   "Alpha", "roman", or "Roman".
#' @param start Starting number or value. Letters are supported if label style
#'   is "alph", "Alph", "alpha", or  "Alpha" and Roman numerals are supported if
#'   label is "roman" or "Roman".
#' @param suffix Suffix character to follow number labels. For example, if `x =
#'   1` and `suffix = "."` the returned label would be "1."
#' @param base Base used in alphabetical number labels. Highest letter to use
#'   for alphabetical numbers. Single digit letters (A to Z) or numbers 1 to 26
#'   are supported. For example, if base is 3, alphabetical labels for numbers
#'   higher than 3 have the prior value prefixed so 3 would be "C" and 4 would
#'   be "AA". Defaults to 26 which converts 27 to "AA", 53 to "BA", etc.
#' @param cols Column name to use for added column for number labels when x is a
#'   data.frame. Defaults to "num_label". If cols is length 2, the first item in
#'   the vector is assumed to be the column name from the data.frame to use as x
#'   and the second item is used as the column name for the added column with
#'   number labels.
#' @inheritParams as_integer
#' @param call Default: [parent.frame()]. Passed to input checking functions
#'   to improve error message traceback.
#' @param pad,side If pad is not `NULL`, pass pad and side to [str_pad()] added
#'   from the [stringstatic::str_pad()] package.
#' @returns
#' - If x is a vector, function returns numeric vector if labels is
#'   "arabic" or a character vector otherwise.
#' - If x is a data.frame, [as_numbered_labels()] returns a modified data.frame
#'   with an added column with a name matching the second value of the cols
#'   parameter.
#' @export
as_numbered_labels <- function(x,
                               labels = "arabic",
                               start = NULL,
                               suffix = NULL,
                               base = 26,
                               cols = "num_label",
                               pad = NULL,
                               side = "left",
                               quiet = TRUE,
                               call = parent.frame()) {
  if (is.data.frame(x)) {
    num_col <- cols
    x_col <- c(1:nrow(x))

    if (length(cols) == 2) {
      num_col <- cols[2]
      check_name(x, cols[1])
      x_col <- x[, cols[1]]
    }

    x[num_col, ] <-
      as_numbered_labels(
        x_col, labels, start, suffix, base, col, pad, side, quiet, call
      )

    return(x)
  }

  if (str_detect(labels, " ")) {
    start <- str_extract(labels, "(?<= ).+$")
    check_nchar(start, n = 1)
    labels <- str_extract(labels, "^.+(?= )")
    labels <- tolower(labels)
    if (str_detect(start, "[A-Z]")) {
      labels <- tosentence(labels)
    }
  }

  labels <-
    match.arg(
      labels, c("arabic", "alph", "Alph", "alpha", "Alpha", "roman", "Roman")
    )

  if (!is.integer(x)) {
    x <- seq_along(x)
  }

  x <- set_start_number(x, start, labels)

  num_labels <-
    switch(labels,
      "arabic" = x,
      "alph" = sapply(x, int_to_alpha,
        base = base,
        dict = letters, quiet = quiet
      ),
      "alpha" = sapply(x, int_to_alpha,
        base = base,
        dict = letters, quiet = quiet
      ),
      "Alph" = sapply(x, int_to_alpha, base = base, quiet = quiet),
      "Alpha" = sapply(x, int_to_alpha, base = base, quiet = quiet),
      "roman" = tolower(as_roman(x, quiet)),
      "Roman" = toupper(as_roman(x, quiet))
    )

  if (!is.null(pad)) {
    num_labels <- str_pad(num_labels, max(nchar(num_labels)), side, pad)
  }

  if (is.null(suffix)) {
    return(num_labels)
  }

  paste0(num_labels, suffix)
}


#' Set start number for numeric vector x
#'
#' Helper for [as_numbered_labels()].
#'
#' @inheritParams as_numbered_labels
#' @export
set_start_number <- function(x, start = NULL, labels = "arabic") {
  if (is.null(start)) {
    start <- 1
  }

  if (!is.numeric(start)) {
    if (labels %in% c("alph", "Alph", "alpha", "Alpha")) {
      start <- alpha_to_int(toupper(start))
    }

    if (labels %in% c("roman", "Roman")) {
      start <- roman_to_int(start)
    }
  }

  x + (start - 1)
}

#' Convert a integer into a corresponding letter or multi-letter string
#'
#' Character values in the provided dict (default to letters "A" to "Z") are
#' passed as is. Non-integer numeric values or characters that are not found in
#' the provided dict are converting to NA values.
#'
#' @source Adapted from the recursive solution provided by G. Grothendieck in [a
#'   May 31, 2017 StackOverflow answer](https://stackoverflow.com/a/44274075).
#'
#' @param x An integer vector or a vector that can be coerced to an integer
#'   vector
#' @param suffix Suffix character to follow alpha character, e.g. if `x = 1` and
#'   `suffix = "."` the returned label would be "A.". suffix is also used to
#'   separate values when x is greater than base, e.g. `x = 27` and `suffix =
#'   "."` returns "A.A." Defaults to `NULL`.
#' @param base If base is not numeric, it is converted to an integer with
#'   [alpha_to_int()].
#' @param dict Character vector to compare to x. Default: LETTERS.
#' @param quiet If `TRUE`, suppress warnings for introduction of NA values
#'   through coercion.
#' @returns An integer vector composed of objects between 1 and 26 with the same
#'   length as x.
#' @export
int_to_alpha <- function(x,
                         suffix = NULL,
                         base = 26,
                         dict = LETTERS,
                         quiet = TRUE) {
  x <- as_integer(x, quiet)

  if (!is.numeric(base)) {
    base <- alpha_to_int(base, dict)
  }

  rest <- (x - 1) %/% base

  alpha <-
    paste0(
      dict[((x - 1) %% base) + 1],
      suffix
    )

  if (rest > 0) {
    return(Recall(rest, alpha, base, dict))
  }

  alpha
}

#' Convert an alphabetical character object from A to Z into a corresponding
#' integer
#'
#' Integers and NA values are passed as is. Double or characters with no
#' corresponding Roman numeral are converting to NA values.
#'
#' @param x Character vector of length n strings to compare to dict. Typically,
#'   letters from "A" to "Z". Case sensitive.
#' @param dict Character vector to match to x. Default: LETTERS.
#' @param n Maximum character length for non-NA objects permitted. Set to NULL
#'   or >1 if dict includes objects with more than one character.
#' @param quiet If `TRUE`, suppress warnings for introduction of NA values
#'   through coercion.
##' @param call Default: [parent.frame()]. Passed to input checking functions
#'   to improve error messages.
#' @returns A length 1 integer between 1 and 26.
#' @export
alpha_to_int <- function(x,
                         dict = LETTERS,
                         n = 1,
                         quiet = TRUE,
                         call = parent.frame()) {
  check_nchar(x, n, call = call)
  x[x %in% dict] <- seq_along(dict)[dict %in% x]
  as_integer(x, quiet)
}

#' Convert a Roman numeral character object into a corresponding integer
#'
#' Integers and NA objects are passed as is. Double numeric objects or
#' characters with no corresponding Roman numeral are converting to NA values.
#'
#' @param x An integer vector or a character vector with characters representing
#'   Roman numerals.
#' @param quiet If `TRUE`, suppress warnings for introduction of NA values
#'   through coercion.
#' @export
roman_to_int <- function(x, quiet = TRUE) {
  as_integer(as_roman(x, quiet), quiet)
}
