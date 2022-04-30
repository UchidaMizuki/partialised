
<!-- README.md is generated from README.Rmd. Please edit that file -->

# partialised

<!-- badges: start -->
<!-- badges: end -->

partialised is an R implementation of function-like object of julia.

## Installation

You can install the development version of partialised from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/partialised")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(partialised)

f <- function(x, y) {
  sqrt(x ^ 2 + y ^ 2)
}

fl <- new_function_partial(f, list(x = 3))

fl
#> <function_partial>
#> function(x, y) {
#>   sqrt(x ^ 2 + y ^ 2)
#> }
#> (
#>   x = 3
#>   ...
#> )
fl(y = 4)
#> [1] 5

arguments(fl)
#> $x
#> [1] 3
arguments(fl)$x <- 6

fl
#> <function_partial>
#> function(x, y) {
#>   sqrt(x ^ 2 + y ^ 2)
#> }
#> (
#>   x = 6
#>   ...
#> )
fl(y = 8)
#> [1] 10

arg(fl, "y")
#> NULL
arg(fl, "y") <- 8

fl()
#> [1] 10
```
