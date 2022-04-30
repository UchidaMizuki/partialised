#' @export
new_function_partial <- function(f,
                                 args = list(), ...,
                                 class = character()) {
  vec_assert(args, list())

  attrs <- list2(...)
  attrs <- attrs[!names(attrs) %in% c("body", "fn")]

  data <- exec(purrr::partial, f, !!!args)
  exec(structure,
       data, !!!attrs,
       class = c(class, "function_partial", class(data)))
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
  out <- call_args(partialised_body(x))
  out[-vec_size(out)]
}

#' @export
`arguments<-` <- function(x, value) {
  attrs <- attributes(x)
  attrs <- attrs[!names(attrs) %in% c("body", "fn")]

  data <- exec(purrr::partial, partialised_fn(x), !!!value)
  exec(structure,
       data, !!!attrs,
       class = class(x))
}

#' @export
arg <- function(x, which) {
  vec_assert(which, character())

  arguments(x)[[which]]
}

#' @export
`arg<-` <- function(x, which, value) {
  vec_assert(which, character())

  arguments(x)[[which]] <- value
  x
}

#' @export
print.function_partial <- function(x, ...) {
  cat_line("<", pillar::type_sum(x), ">")
  print(partialised_fn(x))
  cat_line("(")
  print_args(arguments(x))
  cat_line("  ...")
  cat_line(")")

  invisible(x)
}

print_args <- function(x) {
  nms <- names2(x)
  nms[nms == ""] <- "."
  nms <- pillar::align(nms)
  nms <- paste0("  ", nms, " = ")

  width_old <- getOption("width")
  width <- max(pillar::get_extent(nms))

  options(width = pmax(0, width_old - width))

  out <- purrr::map2(unname(x), nms,
                     function(x, nm) {
                       if (is_scalar_atomic(x) && !is_named(x)) {
                         out <- as.character(x)
                       } else {
                         out <- utils::capture.output(x)
                       }

                       names(out)[[1L]] <- nm
                       names(out)[-1L] <- strrep(" ", width)

                       out
                     })
  out <- vec_c(!!!out)

  options(width = width_old)

  cat_line(names(out), out)

  invisible(x)
}

#' @export
type_sum.function_partial <- function(x) {
  "function_partial"
}
