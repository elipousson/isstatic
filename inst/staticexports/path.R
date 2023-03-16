#' Construct path to file ignoring NULL values for filename or path
#'
#' A replacement for `file.path()`
#'
#' @param ... Additional strings to pass before path and filename.
#' @param filename File name. Optional if path is supplied.
#' @param path Path name. Optional if filename is supplied.
#' @param allow_null If `TRUE`, return `NULL` if filename and path are `NULL`
#'   and no additional strings are provided to .... If `FALSE`, stop if filename
#'   and path are `NULL` and no additional strings are provided to ...
#' @noRd
file_path <- function(...,
                      filename = NULL,
                      path = NULL,
                      fsep = .Platform$file.sep,
                      allow_null = FALSE) {
  path <- str_c(..., path, filename, sep = fsep)

  if (identical(path, character(0)) && isTRUE(allow_null)) {
    return(NULL)
  }

  static_check_character(path)

  path
}
