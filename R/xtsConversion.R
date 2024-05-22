#
#   Routines for conversion to/from xts
#

#'
#'  Convert xts to regression-ready data frame
#'
#'  DEPRECATED. Use tidy() instead.
#'
#'  The three output columns are:
#'  \itemize{
#'      \item \code{date} - Taken from xts index
#'      \item \code{symbol} - Taken from xts column names
#'      \item \code{value} - Taken from the matrix cell at the intersection
#'         of \code{date} and \code{symbol}
#'  }
#'
#' @param x An xts matrix
#' @return A data frame with columns
#'    \code{date}, \code{symbol}, and \code{value}
#'
#' @export
#'
xtsToRegressionData = function(x) {
    decl(x, xts::is.xts)

    dfrm = data.frame(date = index(x),
                      as.data.frame(x) )
    tidyr::gather(dfrm, symbol, value, -date)
}

#'
#'  Convert xts matrix to data frame
#'
#'  DEPRECATED. Use tidy() instead.
#'
#' @param x An xts matrix
#' @return A data frame with the input columns
#'    plus a new 'date' column
#'
#' @export
#'
xtsToDataFrame = function(x) {
    decl(x, xts::is.xts)

    data.frame(
        date=index(x),
        as.data.frame(x) )
}

#'
#'  Convert a data frame to an xts matrix
#'
#"  DEPRECATED. Use as.xts.data.frame or as.xts.tbl_df instead.
#'
#' @param dfrm A data frame with a 'date' column
#' @return An xts matrix
#'
#' @export
#'
dataFrameToXts = function(dfrm) {
    decl(dfrm, is.data.frame)

    stopifnot("date" %in% colnames(dfrm))

    pos = which("date" %in% colnames(dfrm))
    xts::xts(dfrm[,-(pos)], as.Date(dfrm[,pos]))
}
