#'
#'  Smart windowing function for time series
#'
#'  This function is a lot like [window.zoo],
#'  but smarter because `from` can be a number,
#'  meaning that many data points.
#'
#'  If either `from` or `to` is NULL,
#'  then data is not chopped from the respective end.
#'
#' @param x An xts matrix
#' @param from Either a Date, a number of obserations,
#'    or NULL (for all data since start)
#' @param to Either a Date
#'    or NULL (for all data to end)
#' @return An xts matrix
#'
#' @export
#'
tauWindow = function(x, from, to) {
  decl(x, xts::is.xts)
  decl(from, is.null %or% is.numeric %or% lubridate::is.Date)
  decl(to, is.null %or% lubridate::is.Date)

  if (non.null(to)) {
    x = x[index(x) <= to, ]
  }

  if (non.null(from)) {
    if (is.numeric(from)) {
      x = tail(x, from)
    } else {
      x = x[index(x) >= from, ]
    }
  }

  return(x)
}
