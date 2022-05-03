
<!-- README.md is generated from README.Rmd. Please edit that file -->

# partialised

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/partialised)](https://CRAN.R-project.org/package=partialised)
<!-- badges: end -->

partialised provides a ‘partialised’ class that extends the partialising
function of ‘purrr’ by making it easier to change the arguments. This is
similar to the function-like object in ‘Julia’
(<https://docs.julialang.org/en/v1/manual/methods/#Function-like-objects>).

## Installation

You can install the development version of partialised from
[GitHub](https://github.com/) with:

``` r
# the released version from CRAN:
install.packages("partialised")

# the development version from GitHub:
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
#> <partialised[1]>
#> function (x, y) 
#> {
#>     sqrt(x^2 + y^2)
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
pdist$x
#> [1] 3
pdist$y
#> NULL

pdist$x <- 6
pdist(y = 8)
#> [1] 10

pdist$y <- 8
pdist()
#> [1] 10
```
