# Add, remove, or extract file extensions from character vectors

These function uses `stringstatic::str_c()`,
`stringstatic::str_remove()` and `stringstatic::str_extract()` and works
to:

- Add file extensions (or replace existing file extensions) with
  `str_add_fileext()`

- Remove file extensions with `str_remove_fileext()`

- Extract existing file names `str_extract_fileext()` (returning NA
  values if a string has no file extension)

## Usage

``` r
str_add_fileext(string, fileext = NULL)

str_remove_fileext(string, fileext = NULL)

str_extract_fileext(string, fileext = NULL)
```

## Arguments

- string:

  Character vector of any length. Required.

- fileext:

  File extension. Optional. Defaults to `NULL`.

## See also

- [`has_fileext()`](https://elipousson.github.io/isstatic/reference/has_fileext.md)

- [`is_fileext_path()`](https://elipousson.github.io/isstatic/reference/is_fileext_path.md)

## Examples

``` r
str_add_fileext("image", "jpeg")
#> [1] "image.jpeg"

str_remove_fileext(c("file.txt", "word.docx"), "docx")
#> [1] "file.txt" "word"    

str_extract_fileext(c("file1.pdf", "file2"))
#> [1] "pdf" NA   

str_extract_fileext(c("image1.png", "image2.jpeg"), "jpeg")
#> [1] NA     "jpeg"
```
