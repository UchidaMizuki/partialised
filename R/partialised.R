#' Create partialised functions
#'
#' @param f A function.
#' @param args A list of default arguments.
#' @param ... Additional arguments for attributes.
#' @param class Name of subclass.
#'
#' @return A `partialised` function.
#'
#' @seealso [purrr::partial()]
#'
#' @examples
#' dist <- function(x, y) {
#'   sqrt(x ^ 2 + y ^ 2)
#' }
#' pdist <- new_partialised(dist,
#'                          list(x = 3))
#' pdist(y = 4)
#'
#' @export
new_partialised <- function(f,
                            args = list(), ...,
                            class = character()) {
  vec_assert(args, list())

  attrs <- list2(...)
  attrs <- attrs[!names(attrs) %in% c("body", "fn")]

  data <- purrr::partial(as_function(f), !!!args)
  exec(structure, data,
       fn = f, !!!attrs,
       class = c(class, "partialised", class(data)))
}

# from `purrr:::partialised_body()`
partialised_body <- function(x) {
  attr(x, "body")
}

partialised_fn <- function(x) {
  attr(x, "fn")
}

#' Argument lists for partialised functions
#'
#' @param x Partialised function.
#' @param value A list of arguments.
#'
#' @return `arguments()` returns a list of arguments.
#'
#' @name arguments
NULL

#' @export
#' @rdname arguments
arguments <- function(x) {
  out <- call_args(partialised_body(x))
  out[-vec_size(out)]
}

#' @export
#' @rdname arguments
`arguments<-` <- function(x, value) {
  attrs <- attributes(x)
  attrs <- attrs[!names(attrs) %in% c("body", "fn")]

  f <- partialised_fn(x)
  data <- purrr::partial(f, !!!value)
  exec(structure, data,
       fn = f, !!!attrs,
       class = class(x))
}

#' Extract or replace arguments for partialised functions
#'
#' @param x Partialised function.
#' @param i Indices specifying arguments to extract or replace.
#' @param ... Additional arguments.
#' @param value An object, the new value of the argument.
#'
#' @return `[`, `[[` and `$` return arguments.
#'
#' @name extract
NULL

#' @export
#' @rdname extract
`[.partialised` <- function(x, i, ...) {
  arguments(x)[i, ...]
}

#' @export
#' @rdname extract
`[<-.partialised` <- function(x, i, value) {
  arguments(x)[i] <- value
  x
}

#' @export
#' @rdname extract
`[[.partialised` <- function(x, i, ...) {
  arguments(x)[[i, ...]]
}

#' @export
#' @rdname extract
`[[<-.partialised` <- function(x, i, value) {
  arguments(x)[[i]] <- value
  x
}

#' @export
#' @rdname extract
`$.partialised` <- function(x, i) {
  x[[i]]
}

#' @export
#' @rdname extract
`$<-.partialised` <- function(x, i, value) {
  x[[i]] <- value
  x
}

#' @export
names.partialised <- function(x) {
  names(arguments(x))
}

#' @export
print.partialised <- function(x, ...) {
  cat_line("<", obj_sum(x), ">")

  print_fn(x)

  cat_line("(")
  print_args(arguments(x))
  cat_line(strrep(" ", 2L), "...")
  cat_line(")")

  invisible(x)
}

print_fn <- function(x) {
  x <- partialised_fn(x)
  environment(x) <- global_env()
  print(get_expr(x),
        useSource = FALSE)
}

print_args <- function(x) {
  if (!vec_is_empty(x)) {
    nms <- names2(x)
    nms[nms == ""] <- "."
    nms <- pillar::align(nms)
    nms <- paste0(strrep(" ", 2L), nms, " = ")

    opts <- options()
    on.exit(options(opts))

    width <- max(pillar::get_extent(nms))
    options(width = pmax(0, opts$width - width))

    spaces <- strrep(" ", width)
    out <- purrr::map2(unname(x), nms,
                       function(x, nm) {
                         if (is_scalar_atomic(x) && !is_named(x)) {
                           out <- as.character(x)
                         } else {
                           out <- utils::capture.output(x)
                         }

                         names(out)[[1L]] <- nm
                         names(out)[-1L] <- spaces

                         out
                       })
    out <- vec_c(!!!out)
    cat_line(names(out), out)
  }
}

#' @export
type_sum.partialised <- function(x) {
  "partialised"
}

#' @export
obj_sum.partialised <- function(x) {
  paste0(type_sum(x), "(", big_mark(vec_size(arguments(x))), ")")
}
