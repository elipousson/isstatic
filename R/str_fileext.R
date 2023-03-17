# `R/str_fileext.R` is imported from `inst/staticexports/str_fileext.R`. 
# Please edit that file instead.

#' Add, remove, or extract file extensions from character vectors
#'
#' @description
#' These function uses [stringstatic::str_c()], [stringstatic::str_remove()] and
#' [stringstatic::str_extract()] and works to:
#'
#' - Add file extensions (or replace existing file extensions) with
#' [str_add_fileext()]
#' - Remove file extensions with [str_remove_fileext()]
#' - Extract existing file names [str_extract_fileext()] (returning NA values if
#' a string has no file extension)
#'
#' @name str_fileext
#' @param string Character vector of any length. Required.
#' @param fileext File extension. Optional. Defaults to `NULL`.
#' @examples
#' str_add_fileext("image", "jpeg")
#'
#' str_remove_fileext(c("file.txt", "word.docx"), "docx")
#'
#' str_extract_fileext(c("file1.pdf", "file2"))
#'
#' str_extract_fileext(c("image1.png", "image2.jpeg"), "jpeg")
#' @seealso
#' - [isstatic::has_fileext()]
#' - [isstatic::is_fileext_path()]
NULL
#'
#' @name str_add_fileext
#' @rdname str_fileext
#' @export
str_add_fileext <- function(string, fileext = NULL) {
  if (is.null(fileext) || !is.null(fileext) && all(has_fileext(string, fileext))) {
    return(string)
  }

  if (any(has_fileext(string))) {
    string <- str_remove_fileext(string)
  }

  str_c(string, ".", fileext)
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

  str_extract(string, paste0("(?<=\\.)", fileext, "$(?!\\.)"))
}
