# Is x a file or directory?

`is_file()` is a wrapper for
[`base::file.exists()`](https://rdrr.io/r/base/files.html) that allows
the exclusion of directories and returning named vectors. `is_dir()` is
a wrapper for [`base::dir.exists()`](https://rdrr.io/r/base/files2.html)
that supports vector inputs rather than single strings. character(0)
inputs return `FALSE`.

## Usage

``` r
is_file(x, include_dirs = FALSE, use_names = FALSE)

is_dir(x, use_names = FALSE)
```

## Arguments

- include_dirs:

  If `TRUE`, return `TRUE` for any value of x that is a directory path.
  If `FALSE` (default), return `FALSE` for directory paths.

- use_names:

  If `TRUE`, return a logical vector where the names match the values of
  the input vector x. Defaults to `FALSE`.
