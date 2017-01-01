#
#   Functions to reorigin data to zero
#

#'
#'  Reorigin data to zero
#'
#'  Translate data so that first element is zero
#'
#' @param x An R object
#' @return Same object, but reorigined to zero
#' @export
#'
reorigin = function(x, ...) UseMethod("reorigin", x)

#' @export
reorigin.default = function(x, ...) stop("Cannot reorigin object of class ", class(x))

#' @export
reorigin.numeric = function(x) {
    x - x[[1]]
}

#' @export
reorigin.matrix = function(m) {
    reorg = function(v) v - v[[1]]

    z = apply(m, 2, reorg)
    colnames(z) = colnames(m)
    return(z)
}

#' @export
reorigin.data.frame = function(dfrm) {
    reorg = function(col) {
        switch(class(col),
               numeric = col - col[[1]],
               integer = col - col[[1]],
               col )
    }

    z = lapply(dfrm, reorg)
    do.call(data.frame, z)
}
