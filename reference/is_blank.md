# Test if a character vector consists of blank strings

Return a logical vector indicating if elements of a character vector are
blank (white spaces or empty strings).

## Usage

``` r
is_blank(x)
```

## Source

Adapted from
[`xfun::is_blank()`](https://rdrr.io/pkg/xfun/man/is_blank.html) in the
[xfun](https://yihui.org/xfun/) package.

## Arguments

- x:

  A character vector.

## Value

`TRUE` for blank elements, or `FALSE` otherwise.

## Author

Yihui Xie <xie@yihui.name>
([ORCID](https://orcid.org/0000-0003-0645-5666))

## Examples

``` r
is_blank("")
#> [1] TRUE
is_blank("abc")
#> [1] FALSE
is_blank(c("", "  ", "\n\t"))
#> [1] TRUE
is_blank(c("", " ", "abc"))
#> [1] FALSE
```
