# `R/path.R` is imported from `inst/staticexports/path.R`. 
# Please edit that file instead.

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
#' @export
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

#' Is x a file or directory?
#'
#' [is_file()] is a wrapper for [base::file.exists()] that allows the exclusion
#' of directories and returning named vectors. [is_dir()] is a wrapper for
#' [base::dir.exists()] that supports vector inputs rather than single strings.
#' character(0) inputs return `FALSE`.
#'
#' @param include_dirs If `TRUE`, return `TRUE` for any value of x that is a
#'   directory path. If `FALSE` (default), return `FALSE` for directory paths.
#' @param use_names If `TRUE`, return a logical vector where the names match the
#'   values of the input vector x. Defaults to `FALSE`.
#' @rdname is_file
#' @export
is_file <- function(x,
                    include_dirs = FALSE,
                    use_names = FALSE) {
  files <- file.exists(x)

  if (use_names) {
    files <- setNames(files, x)
  }
  if (!include_dirs) {
    files <- files & !is_dir(x)
  }

  if (identical(files, logical(0))) {
    return(FALSE)
  }

  files
}

#' @name is_dir
#' @rdname is_file
#' @export
is_dir <- function(x, use_names = FALSE) {
  dirs <-
    vapply(
    x,
    function(p) {
      dir.exists(p)
    },
    FUN.VALUE = TRUE,
    USE.NAMES = use_names
  )

  if (identical(dirs, logical(0))) {
    return(FALSE)
  }

  dirs
}
