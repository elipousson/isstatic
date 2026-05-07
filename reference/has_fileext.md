# Does string contain the specified file type or any file extension?

Check if string contains any filetype or the provided filetype. If
string is `NULL`, returns `FALSE`.

## Usage

``` r
has_fileext(string = NULL, fileext = NULL, ignore.case = FALSE)
```

## Arguments

- string:

  String to be tested with or without filetype. Defaults to `NULL`.

- fileext:

  File type to test against. Optional.

- ignore.case:

  If `FALSE`, the pattern matching is case sensitive. If `TRUE`, case is
  ignored.

## See also

[`is_fileext_path()`](https://elipousson.github.io/isstatic/reference/is_fileext_path.md)
