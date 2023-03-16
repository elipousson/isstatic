#' Static checks
#'
#' @param condition Condition to check.
#' @param message Message to pass to stop if condition is FALSE. Defaults to
#'   `NULL`.
#' @param call Call passed to stop. Defaults to parent.frame()
#' @name static_check
#' @keywords internal
NULL

#' @name static_check_if
#' @rdname static_check
#' @noRd
static_check_if <- function(condition, message = NULL, call = parent.frame()) {
  if (isTRUE(condition)) {
    return(invisible(NULL))
  }

  stop(
    message,
    call. = call
  )
}

#' @name static_check_character
#' @rdname static_check
#' @noRd
static_check_character <- function(x, call = parent.frame()) {
  static_check_if(
    condition = all(is.character(x[!is.na(x)])),
    message = paste("`x` must be a <character> vector, not", class(x)),
    call = call
  )
}

#' @name static_check_numeric
#' @rdname static_check
#' @noRd
static_check_numeric <- function(x, call = parent.frame()) {
  static_check_if(
    condition = all(is.numeric(x[!is.na(x)])),
    message = paste("`x` must be a <numeric> vector, not", class(x)),
    call = call
  )
}

#' @name static_check_nchar
#' @rdname static_check
#' @noRd
static_check_nchar <- function(x, n = 1, ..., call = parent.frame()) {
  num_char <- unique(nchar(x[!is.na(x)], ...))

  message <- num_char

  if (length(num_char) > 1) {
    message <- paste("a range from", min(num_char), "to", max(num_char))
  }

  message <- paste0(
    "All objects in `x` must have ", n, plural_words(" character", n),
    ", not ", message, "."
  )

  static_check_if(
    condition = is.null(n) | all(n == num_char),
    message = message,
    call = call
  )
}

#' @name static_check_name
#' @rdname static_check
#' @noRd
static_check_name <- function(x, name = NULL, call = parent.frame()) {
  static_check_if(
    condition = has_all_names(x, name),
    message = paste0(
      "`x` must have ", plural_words("name", length(name), after = " "), name,
      ", but ", combine_words(name[!(name %in% names(x))]), " are all missing."
    ),
    call = call
  )
}
