# Do any items in a list or vector return `TRUE` from a predicate function?

Do any items in a list or vector return `TRUE` from a predicate
function?

- `is_any_null()`: Is any item in a list or vector a `NULL` value?

&nbsp;

- `is_any_na()`: Is any item in a list or vector a `NA` value?

&nbsp;

- `is_none()`: Is no item in a list or vector return `TRUE` from a
  predicate function?

&nbsp;

- `is_none_null()`: Is no item in a list or vector is `NULL`?

Do any items in a list or vector return `TRUE` from a predicate
function?

- `is_any_null()`: Is any item in a list or vector a `NULL` value?

&nbsp;

- `is_any_na()`: Is any item in a list or vector a `NA` value?

## Usage

``` r
is_any(x, FUN, ...)

is_any_null(x)

is_any_na(x)

is_none(x, FUN, ...)

is_none_null(x)

is_any(x, FUN, ...)

is_any_null(x)

is_any_na(x)
```

## Arguments

- x:

  A list or vector passed to
  [`vapply()`](https://rdrr.io/r/base/lapply.html).

- FUN:

  the function to be applied to each element of `X`: see ‘Details’. In
  the case of functions like `+`, `%*%`, the function name must be
  backquoted or quoted.

- ...:

  Arguments passed on to
  [`base::vapply`](https://rdrr.io/r/base/lapply.html),
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

`TRUE` if FUN returns `TRUE` for any element of x or `FALSE` if all
elements return `FALSE`.

`TRUE` if FUN returns `TRUE` for any element of x or `FALSE` if all
elements return `FALSE`.

## See also

[`is_all()`](https://elipousson.github.io/isstatic/reference/is_all.md)

[`is_all()`](https://elipousson.github.io/isstatic/reference/is_all.md)
