#'
#'  Forward difference of a time series
#'
#' @param x zoo or xts matrix
#' @param lag numeric; period to difference over
#' @param na.pad logical
#' @return A matrix (same type as input) of forward differences
#'
#' @export
#'
fwdDiff = function(x, lag, na.pad=TRUE) {
  decl(x, is.zoo)
  decl(lag, is.numeric)
  decl(na.pad, is.logical)

  stopifnot(lag >= 0)

  (xts::diff.xts(x, lag=lag)
   |> xts::lag.xts(k=-(lag)) )
}
