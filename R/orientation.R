#
#   Handy functions for forcing matrices and data frames
#   into landscape or portrait orientation
#

#'
#'  Orient a data structure into landscape
#'
#'  This is handy for forcing a matrix or data frame
#'  into landscape orientation prior to printing.
#'
#' @param x A matrix or data frame
#' @return The input (`x`) in landscape orientation
#'
#' @export
#'
landscape = function(x, ...) orient(x, "landscape", ...)

#'
#'  Force a data structure into portrait orientation
#'
#'  This is handy for forcing a matrix or data frame
#'  into portrait orientation prior to printing.
#'
#' @param x A matrix or data frame
#' @return The input (`x`) in portrait orientation
#'
#' @export
#'
portrait = function(x, ...) orient(x, "portrait", ...)

# ----------------------------------------------------------

#' @export
orient = function(x, ...) UseMethod("orient", x)

#' @export
orient.default = function(x, which) {
    switch(which,
           landscape = rbind(x),
           portrait = cbind(x),
           stop("Invalid orientation: ", which) )
}

#' @export
orient.numeric = function(x, which) {
    switch(which,
           landscape = {
             vec = rbind(x)
             rownames(vec) = NULL
           },
           portrait = {
             vec = cbind(x)
             colnames(vec) = NULL
           },
           stop("Invalid orientation: ", which) )
    return(vec)
}

#' @export
orient.matrix = function(x, which) {
    isOK = switch(which,
                  landscape = (ncol(x) >= nrow(x)),
                  portrait = (ncol(x) <= nrow(x)),
                  stop("Invalid orientation: ", which) )

    if (!isOK) {
      # Note: t() correctly swaps rownames and colnames
      x = t(x)
    }
    return(x)
}

#' @export
orient.data.frame = function(x, way) {
    isOK = switch(way,
                  landscape = (ncol(x) >= nrow(x)),
                  portrait = (ncol(x) <= nrow(x)),
                  stop("Invalid orientation: ", way) )

    if (!isOK) {
        # Note #1: t() will force conversion to matrix
        # Note #2: t() correctly swaps rownames and colnames
        x = as.data.frame(t(x))

    }
    return(x)
}
