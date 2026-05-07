# Static checks

Static checks

## Usage

``` r
static_check_if(condition, message = NULL, call = parent.frame())

static_check_character(x, call = parent.frame())

static_check_numeric(x, call = parent.frame())

static_check_nchar(x, n = 1, ..., call = parent.frame())

static_check_name(x, name = NULL, call = parent.frame())
```

## Arguments

- condition:

  Condition to check.

- message:

  Message to pass to stop if condition is FALSE. Defaults to `NULL`.

- call:

  Call passed to stop. Defaults to parent.frame()
