# Get the n most frequent or least frequent appearing values in a vector

This function does not address ties in frequency.

## Usage

``` r
str_n_freq(string = NULL, n = NULL, decreasing = TRUE)
```

## Arguments

- string:

  A character vector.

- n:

  The number of values to return based on frequency of appearance.
  Defaults to `NULL` which returns all unique values from x.

- decreasing:

  Passed to [`sort()`](https://rdrr.io/r/base/sort.html)
