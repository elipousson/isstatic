# Does this text end in the specified filetype?

Does this text end in the specified filetype?

## Usage

``` r
is_filetype_path(x, filetype, ignore.case = TRUE)
```

## Arguments

- x:

  A character vector to check for matches, or an object which can be
  coerced by [`as.character()`](https://rdrr.io/r/base/character.html)
  to a character vector.

- filetype:

  A file extension (or multiple file extensions) to compare to x.
  Required.

- ignore.case:

  logical. if `FALSE`, the pattern matching is *case sensitive* and if
  `TRUE`, case is ignored during matching.
