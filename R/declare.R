#'
#'  Type-check a function parameter
#'
#'  Stops if variable type is wrong.
#'
#'  `decl` is a simple, fast type checker,
#'  which is nice when speed matters,
#'  such as in low-level functions.
#'
#' @param `x` Variable to be type-checked
#' @param `pred` A type checking predicate, such as
#'   `is.character` or `is.data.frame` (function)
#' @return `decl` returns its first argument,
#'   but halts on type errors.
#' @export
#'
decl = function(x, pred) {
  if (!pred(x)) {
    caller <- as.list(sys.call(-1))[[1]]
    fatal("Type is", class(x)[[1]], "instead of", substitute(pred),
          caller = caller )
  }
  return(x)
}
