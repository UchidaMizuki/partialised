#' @export
new_function_like <- function(f, args, ...,
                              class = character()) {
  vec_assert(args, list())

  attrs <- list2(...)
  attrs <- attrs[!names(attrs) %in% c("body", "fn", "args")]

  data <- exec(purrr::partial, f, !!!args)
  exec(structure,
       data,
       args = args, !!!attrs,
       class = c(class, "function_like", class(data)))
}

# from `purrr:::partialised_body()`
partialised_body <- function(x) {
  attr(x, "body")
}

# from `purrr:::partialised_fn()`
partialised_fn <- function(x) {
  attr(x, "fn")
}

#' @export
arguments <- function(x) {
  attr(x, "args")
}

#' @export
`arguments<-` <- function(x, value) {
  attrs <- attributes(x)
  attrs <- attrs[!names(attrs) %in% c("body", "fn", "args")]

  data <- exec(purrr::partial, partialised_fn(x), !!!value)
  exec(structure,
       data,
       args = value, !!!attrs,
       class = class(x))
}

#' @export
print.function_like <- function(x, ...) {
  cat("<", pillar::type_sum(x), ">\n",
      sep = "")

  # from `purrr:::print.purrr_function_partial()`
  out <- x
  body <- quo_squash(partialised_body(out))
  body[[1L]] <- partialised_fn(out)
  body(out) <- body
  out <- set_env(out, global_env())
  print(out, ...)

  invisible(x)
}

#' @export
type_sum.function_like <- function(x) {
  "function_like"
}
