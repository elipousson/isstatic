# Do all items in a list or vector return TRUE from a predicate function?

Do all items in a list or vector return TRUE from a predicate function?

- `is_all_null()`: Are all items in a list or vector `NULL` values?

&nbsp;

- `is_all_na()`: Are all items in a list or vector NA values?

## Usage

``` r
is_all(x, FUN, ...)

is_all_null(x)

is_all_na(x)
```

## Arguments

- x:

  A list or vector passed to X parameter of
  [`vapply()`](https://rdrr.io/r/base/lapply.html).

- FUN:

  the function to be applied to each element of `X`: see ‘Details’. In
  the case of functions like `+`, `%*%`, the function name must be
  backquoted or quoted.

- ...:

  Arguments passed on to
  [`base::vapply`](https://rdrr.io/r/base/lapply.html)

  `USE.NAMES`

  :   logical; if `TRUE` and if `X` is character, use `X` as
      [`names`](https://rdrr.io/r/base/names.html) for the result unless
      it had names already. Since this argument follows `...` its name
      cannot be abbreviated.

  `FUN.VALUE`

  :   a (generalized) vector; a template for the return value from FUN.
      See ‘Details’.

## Value

`TRUE` if FUN returns `TRUE` for all elements of x or `FALSE` if any
element returns `FALSE`.

## See also

[`is_any()`](https://elipousson.github.io/isstatic/reference/is_any.md)
