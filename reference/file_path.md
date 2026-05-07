# Construct path to file ignoring NULL values for filename or path

A replacement for [`file.path()`](https://rdrr.io/r/base/file.path.html)

## Usage

``` r
file_path(
  ...,
  path = NULL,
  filename = NULL,
  fsep = .Platform$file.sep,
  allow_null = FALSE,
  call = parent.frame()
)
```

## Arguments

- ...:

  Additional strings to pass before path and filename.

- path:

  Path name. Optional if filename is supplied.

- filename:

  File name. Optional if path is supplied.

- fsep:

  the path separator to use (assumed to be ASCII).

- allow_null:

  If `TRUE`, return `NULL` if filename and path are `NULL` and no
  additional strings are provided to .... If `FALSE`, stop if filename
  and path are `NULL` and no additional strings are provided to ...
