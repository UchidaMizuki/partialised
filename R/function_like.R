#' @export
new_function_like <- function(f,
                              args = list(), ...,
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
  print(partialised_fn(x))
  cat("(\n")
  print_args(arguments(x))
  cat("  ...\n")
  cat(")\n")

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

  cat(paste0(names(out), out),
      sep = "\n")

  invisible(x)
}

#' @export
type_sum.function_like <- function(x) {
  "function_like"
}
