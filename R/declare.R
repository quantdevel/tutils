#'
#'  Type-check a function parameter
#'
#'  Stops if variable type is wrong.
#'
#'  \code{decl} is a simple, fast type checker,
#'  and should be used when speed matters,
#'  such as in low-level functions.
#'  \code{declare} is more powerful and implements
#'  a convenient syntax for richer types.
#'
#' @param ... Parameters of the form name="type"
#'   or even name="type1|type2|..."
#' @param x Variable to be type-checked
#' @param pred A type checking predicate, such as
#'   \code{is.character} or \code{is.data.frame} (function)
#' @return \code{decl} returns its first argument.
#'   \code{declare} returns NULL invisibly.
#'   In any event, both functions halt on type errors.
#' @export
#'
declare = function(..., ..env = parent.frame()) {

    parseTypes = function(s) {
        if (!is.character(s)) stop("Invalid type specification: ", str(s))
        strsplit(s, "|", fixed=TRUE)[[1]]
    }

    checkTypes = function(name, types, value) {
        checkType = function(type) {
            if (type == "number")
                inherits(value, "numeric") || inherits(value, "integer")
            else
                inherits(value, type)
        }
        if (any(sapply(types, checkType))) return(NULL)
        classes <- paste(class(value), sep=", ")
        stop("Argument '", name, "' is type ", classes,
             ", not type ", paste(types,collapse="|"))
    }

    params <- list(...)
    for (name in names(params)) {
        types <- parseTypes(params[[name]])
        value <- get(name, ..env)
        checkTypes(name, types, value)
    }
    return(invisible(NULL))
}

#' @rdname declare
#' @export
decl = function(x, pred) {
  if (!pred(x)) {
    caller <- as.list(sys.call(-1))[[1]]
    fatal("Type is", class(x)[[1]], "instead of", substitute(pred),
          caller = caller )
  }
  return(x)
}
