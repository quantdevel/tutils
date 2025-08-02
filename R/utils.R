#'
#'  Add free newline to cat arguments
#'
#' @param ... Arguments to cat
#' @return Nothing. Called for side-effect
#' @export
#'
catln = function(...) {
  cat(..., "\n")
}

#'
#'  Test for non-NA
#'
#'  Return TRUE if argument is not NA, FALSE otherwise
#'
#' @param x Any R object
#' @return TRUE if `x` is not NA, FALSE otherwise.
#'
#' @export
#'
non.na = function(x) {
  !is.na(x)
}

#'
#'  Test for non-NULL
#'
#'  Return TRUE if argument is not NULL, FALSE otherwise
#'
#' @param x Any R object
#' @return TRUE if `x` is not NULL, FALSE otherwise.
#'
#' @export
#'
non.null = function(x) {
  !is.null(x)
}

#'
#' @title Set column names in a pipe
#'
#' @description
#' This is like assigning column names, but in a pipe-friendly way.
#'
#' @param x Anything that has column names
#' @param names Character vector of new column names
#' @seealso [magrittr::set_colnames], from which this idea was stolen
#' @export
#'
setColnames = function(x, names) {
    colnames(x) <- names
    return(x)
}

#'
#'  Return first (and only) element of argument
#'
#'  Returns first element of argument.
#'  Stops if argument does not have exactly one element.
#'
#' @param x A vector or list
#' @return First element of x
#' @export
#'
the = function(x) {
    stopifnot(length(x) == 1)
    dimx = dim(x)
    if (is.null(dimx)) return(x[[1]])
    if (length(dimx) == 1) return(x[[1]])
    if (length(dimx) == 2) return(x[[1,1]])
    stop("Cannot apply 'the' to multi-dimensional structures")
}

#'
#'  Flip a matrix or data frame, top to bottom
#'
#' @param m A matrix or data frame
#' @return The input, but with rows in opposite order
#' @export
#'
upsideDown = function(m) {
    m[nrow(m):1,]
}
