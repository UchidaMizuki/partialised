
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

dist <- function(x, y) {
  sqrt(x ^ 2 + y ^ 2)
}

pdist <- new_partialised(dist,
                         list(x = 3))

pdist
#> <partialised(1)>
#> function(x, y) {
#>   sqrt(x ^ 2 + y ^ 2)
#> }
#> (
#>   x = 3
#>   ...
#> )
pdist(y = 4)
#> [1] 5

arguments(pdist)
#> $x
#> [1] 3
arguments(pdist)$x <- 6

pdist
#> <partialised(1)>
#> function(x, y) {
#>   sqrt(x ^ 2 + y ^ 2)
#> }
#> (
#>   x = 6
#>   ...
#> )
pdist(y = 8)
#> [1] 10

arg(pdist, "y")
#> NULL
arg(pdist, "y") <- 8

pdist
#> <partialised(2)>
#> function(x, y) {
#>   sqrt(x ^ 2 + y ^ 2)
#> }
#> (
#>   x = 6
#>   y = 8
#>   ...
#> )
pdist()
#> [1] 10
```
