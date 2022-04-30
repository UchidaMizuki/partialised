#' @export
new_function_like <- function(f, args, ...,
                              class = character()) {
  vec_assert(args, list())

  data <- exec(purrr::partial, f, !!!args)
  structure(data,
            args = args,
            class = c(class, "function_like", class(data)))
}

#' @export
arguments <- function(f) {
  attr(f, "args")
}

#' @export
`arguments<-` <- function(f, value) {
  data <- exec(purrr::partial, partialised_fn(f), !!!value)
  structure(data,
            args = value,
            class = class(f))
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
