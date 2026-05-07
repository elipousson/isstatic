# Convert a integer into a corresponding letter or multi-letter string

Character values in the provided dict (default to letters "A" to "Z")
are passed as is. Non-integer numeric values or characters that are not
found in the provided dict are converting to NA values.

## Usage

``` r
int_to_alpha(x, suffix = NULL, base = 26, dict = LETTERS, quiet = TRUE)
```

## Source

Adapted from the recursive solution provided by G. Grothendieck in [a
May 31, 2017 StackOverflow
answer](https://stackoverflow.com/a/44274075).

## Arguments

- x:

  An integer vector or a vector that can be coerced to an integer vector

- suffix:

  Suffix character to follow alpha character, e.g. if `x = 1` and
  `suffix = "."` the returned label would be "A.". suffix is also used
  to separate values when x is greater than base, e.g. `x = 27` and
  `suffix = "."` returns "A.A." Defaults to `NULL`.

- base:

  If base is not numeric, it is converted to an integer with
  [`alpha_to_int()`](https://elipousson.github.io/isstatic/reference/alpha_to_int.md).

- dict:

  Character vector to compare to x. Default: LETTERS.

- quiet:

  If `TRUE`, suppress warnings for introduction of NA values through
  coercion.

## Value

An integer vector composed of objects between 1 and 26 with the same
length as x.
