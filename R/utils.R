# from `purrr:::partialised_body()`
partialised_body <- function(x) {
  attr(x, "body")
}

# from `purrr:::partialised_fn()`
partialised_fn <- function(x) {
  attr(x, "fn")
}
