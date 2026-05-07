# Combine multiple words into a single string

This is a wrapper function of
[`xfun::join_words()`](https://rdrr.io/pkg/xfun/man/join_words.html).

## Usage

``` r
combine_words(
  words,
  sep = ", ",
  and = " and ",
  before = "",
  after = before,
  oxford_comma = TRUE
)
```

## Source

Adapted from
[`knitr::combine_words()`](https://rdrr.io/pkg/knitr/man/combine_words.html)
in the [knitr](https://yihui.org/knitr/) package.

## Value

A character string

## Author

Yihui Xie <xie@yihui.name>
([ORCID](https://orcid.org/0000-0003-0645-5666))
