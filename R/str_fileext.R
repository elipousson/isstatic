# `R/str_fileext.R` is imported from `inst/staticexports/str_fileext.R`. 
# Please edit that file instead.

#' Modify strings to help make consistent file names
#'
#' Functions include:
#'
#' - [str_add_fileext()]: Add file type to string
#' - [str_remove_fileext()]: Remove file type from string
#' - [str_extract_fileext()]: Extract file type from string
#'
#' @name str_fileext
#' @param string Character vector
NULL
#'
#' @name str_add_fileext
#' @rdname str_fileext
#' @param fileext File extension string
#' @export
str_add_fileext <- function(string, fileext = NULL) {
  if (!is.null(fileext) & all(has_fileext(string, fileext))) {
    return(string)
  }

  if (any(has_fileext(string))) {
    string <- str_remove_fileext(string)
  }

  paste0(string, ".", fileext)
}

#' @name str_remove_fileext
#' @rdname str_fileext
#' @export
str_remove_fileext <- function(string, fileext = NULL) {
  if (is.null(fileext)) {
    fileext <- str_extract_fileext(string)
  }

  str_remove(string, paste0("\\.", fileext, "$"))
}

#' @name str_extract_fileext
#' @rdname str_fileext
#' @export
str_extract_fileext <- function(string, fileext = NULL) {
  if (is.null(fileext)) {
    fileext <- "[a-zA-Z0-9]+"
  }
  regmatches(
    string,
    regexpr(paste0("(?<=\\.)", fileext, "$(?!\\.)"), string, perl = TRUE)
    )
}
