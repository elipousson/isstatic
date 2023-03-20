#' Construct path to file ignoring NULL values for filename or path
#'
#' A replacement for `file.path()`
#'
#' @param ... Additional strings to pass before path and filename.
#' @param path Path name. Optional if filename is supplied.
#' @param filename File name. Optional if path is supplied.
#' @inheritParams base::file.path
#' @param allow_null If `TRUE`, return `NULL` if filename and path are `NULL`
#'   and no additional strings are provided to .... If `FALSE`, stop if filename
#'   and path are `NULL` and no additional strings are provided to ...
#' @noRd
file_path <- function(...,
                      path = NULL,
                      filename = NULL,
                      fsep = .Platform$file.sep,
                      allow_null = FALSE,
                      call = parent.frame()) {
  path <- str_c(..., path, filename, sep = fsep)
  path_has_null <- any(identical(path, character(0)))

  if (isTRUE(path_has_null) && isTRUE(allow_null)) {
    return(NULL)
  }

  if (isFALSE(all(is.character(path)) && isFALSE(path_has_null))) {
    stop(
      "! `file_path()` must return a character vector when `allow_null = FALSE`.",
      call. = call
    )
  }

  path
}
