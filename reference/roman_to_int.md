# Convert a Roman numeral character object into a corresponding integer

Integers and NA objects are passed as is. Double numeric objects or
characters with no corresponding Roman numeral are converting to NA
values.

## Usage

``` r
roman_to_int(x, quiet = TRUE)
```

## Arguments

- x:

  An integer vector or a character vector with characters representing
  Roman numerals.

- quiet:

  If `TRUE`, suppress warnings for introduction of NA values through
  coercion.
