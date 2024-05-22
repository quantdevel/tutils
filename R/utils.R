#'
#'  Add free newline to cat arguments
#'
#' @param ... Arguments to cat
#' @return Nothing. Called for side-effect
#' @export
#'
catln = function(...) cat(..., "\n")

#'
#'  Return first non-NULL, non-NA argument
#'
#' @param ... Just arguments
#' @return First nont-NULL, non-NA value
#' @export
#'
coalesce = function(...) {
    for (x in list(...)) {
        if (!is.null(x) && !is.na(x)) return(x)
    }
    NULL
} 

#'
#'  Bind list elements into columns - OBSOLETE
#'
#'  OBSOLETE. Replaced by dplyr::bind_cols.
#'
#'  This is handy with the pipe operator:
#'  aList |> lapply(aFunction) |> do.cbind()
#'
#' @param lst A list
#' @return A matrix whose columns are taken from the list elements
#' @seealso \link{do.rbind}
#'
#' @export
#'
do.cbind = function(lst) do.call(cbind, lst)

#'
#'  Bind list elements into rows - OBSOLETE
#'
#'  OBSOLETE. Replaced by dplyr::bind_rows.
#'
#'  This is handy with the pipe operator:
#'  alist |> lapply(aFunction) |> do.rbind()
#'
#' @param lst A list
#' @return A matrix whose rows are taken from the list elements
#' @seealso \link{do.cbind}
#'
#' @export
#'
do.rbind = function(lst) do.call(rbind, lst)

#'
#'  Test for non-NA
#'
#'  Return TRUE if argument is not NA, FALSE otherwise
#'
#' @param x Any R object
#' @return TRUE if \code{x} is not NA, FALSE otherwise.
#'
#' @export
#'
non.na = function(x) !is.na(x)

#'
#'  Test for non-NULL
#'
#'  Return TRUE if argument is not NULL, FALSE otherwise
#'
#' @param x Any R object
#' @return TRUE if \code{x} is not NULL, FALSE otherwise.
#'
#' @export
#'
non.null = function(x) !is.null(x)

#'
#'  Return Nth item of a vector or Nth row of a 2-dimensional structure
#'
#' @param x Vector, matrix, or data frame
#' @param n Index of item or row
#' @return Nth item of vector, or Nth row of matrix or data frame
#' @export
#'
nth = function(x, n) {
  dims = dim(x)
  if (is.null(dims)) {
    x[[n]]
  } else if (length(dims) == 2) {
    x[n,]
  } else {
    stop("Cannot take Nth item of multi-dimensional structure")
  }
}

#' @export
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
