#'
#'  Smart windowing function for time series
#'
#'  This function is a lot like \link{window.zoo},
#'  but smarter because \code{from} can be a number,
#'  meaning that many data points.
#'
#'  If either \code{from} or \code{to} is NULL,
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
  declare(x="xts")
  declare(from="integer|numeric|Date|NULL")
  declare(to="Date|NULL")

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
