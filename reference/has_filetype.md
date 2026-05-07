# Does string contain the specified file type or any file extension?

Alternate naming convention for
[`has_fileext()`](https://elipousson.github.io/isstatic/reference/has_fileext.md)

## Usage

``` r
has_filetype(string = NULL, filetype = NULL, ignore.case = FALSE)
```

## Arguments

- string:

  String to be tested with or without filetype. Defaults to `NULL`.

- filetype:

  File type to test against. Optional.

- ignore.case:

  If `FALSE`, the pattern matching is case sensitive. If `TRUE`, case is
  ignored.
