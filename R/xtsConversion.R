#
#   Routines for conversion to/from xts
#

#'
#'  Convert xts to regression-ready data frame
#'
#'  Use this when then xts column names are factor levels,
#'  such as stock symbols or names of futures contracts.
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
    declare(x="xts")

    dfrm = data.frame(date = index(x),
                      as.data.frame(x) )
    tidyr::gather(dfrm, symbol, value, -date)
}

#'
#'  Convert xts matrix to data frame
#'
#'  Adds 'date' column to hold the xts index
#'
#' @param x An xts matrix
#' @return A data frame with the input columns
#'    plus a new 'date' column
#'
#' @import zoo
#' @export
#'
xtsToDataFrame = function(x) {
    declare(x="xts")

    data.frame(
        date=index(x),
        as.data.frame(x) )
}

#'
#'  Convert a data frame to an xts matrix
#'
#'  This assumes that the input contains a 'date' column
#'    with the needed index. The column will be converted
#'    to Date objects, if necessary.
#'
#' @param dfrm A data frame with a 'date' column
#' @return An xts matrix
#'
#' @export
#'
dataFrameToXts = function(dfrm) {
    declare(dfrm="data.frame")

    stopifnot("date" %in% colnames(dfrm))

    pos = which("date" %in% colnames(dfrm))
    xts::xts(dfrm[,-(pos)], as.Date(dfrm[,pos]))
}
