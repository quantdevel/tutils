#'
#'  Type-check a function parameter
#'  
#'  Stops if parameter type is wrong
#'  
#' @param ... Parameters of the form name="type"
#' @return Nothing. Called for (possible) side-effect
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
        stop("Argument '", name, "' is type ", classes, ", not type ", paste(types,collapse="|"))
    }

    params <- list(...)
    for (name in names(params)) {
        types <- parseTypes(params[[name]])
        value <- get(name, ..env)
        checkTypes(name, types, value)
    }
    return(invisible(NULL))
}
