# Convert an alphabetical character object from A to Z into a corresponding integer

Integers and NA values are passed as is. Double or characters with no
corresponding Roman numeral are converting to NA values.

## Usage

``` r
alpha_to_int(x, dict = LETTERS, n = 1, quiet = TRUE, call = parent.frame())
```

## Arguments

- x:

  Character vector of length n strings to compare to dict. Typically,
  letters from "A" to "Z". Case sensitive.

- dict:

  Character vector to match to x. Default: LETTERS.

- n:

  Maximum character length for non-NA objects permitted. Set to NULL or
  \>1 if dict includes objects with more than one character.

- quiet:

  If `TRUE`, suppress warnings for introduction of NA values through
  coercion.

- call:

  Default: [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html).
  Passed to input checking functions to improve error messages.

## Value

A length 1 integer between 1 and 26.
