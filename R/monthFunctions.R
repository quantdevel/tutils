#
# Functions for working with months
#

#'
#'  Create month factor from zoo or xts object
#'
#'  Given a zoo or xts object, return a month factor
#'  based on the index. Factor levels are Jan, Feb, Mar, ...
#'
#'  TODO: Really, there should be monthFactor.zoo,
#'    monthFactor.xts, monthFactor.Date, monthFactor.character, monthFactor.integer, etc.
#'
#' @param z The zoo or xts object
#' @return Factor with levels Jan, Feb, Mar, ...
#'
#' @seealso months
#' @export
#'
monthFactor <- function(z) {
  ensure(z, zoo::is.zoo(.))

	monthLevels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	factor(months(zoo::index(z), abbreviate=TRUE),
		     levels=monthLevels )
}

#'
#'  Convert month in character format to number (1, ..., 12)
#'
#' @param x Can be a number (1, ..., 12), name ("jan", "feb", ...),
#'    futures month code ("F", "G", "H", ...),
#'    Date, or yearmon.
#'    Case is ignored for character arguments.
#' @return Month number (1, ..., 12)
#'
#' @export
#'
monthNumber = function(x, ...) UseMethod("monthNumber", x)

#' @rdname monthNumber
#' @export
monthNumber.default = function(x) {
  stop("Cannot interpret ", class(x), " as month")
}

#' @rdname monthNumber
#' @export
monthNumber.integer = function(x) x

#' @rdname monthNumber
#' @export
monthNumber.numeric = function(x) as.integer(x)

#' @rdname monthNumber
#' @export
monthNumber.character = function(s) {
  map = list(
    f = 1, g = 2, h = 3,
    j = 4, k = 5, m = 6,
    n = 7, q = 8, u = 9,
    v = 10, x = 11, z = 12,
    jan = 1, feb = 2, mar = 3,
    apr = 4, may = 5, jun = 6,
    jul = 7, aug = 8, sep = 9,
    oct = 10, nov = 11, dec = 12 )

  num = map[tolower(s)]
  if (any(sapply(num, is.null))) {
    stop("Cannot translate month name(s): ", s[sapply(num, is.null)])
  }
  as.integer(unlist(num))   # Strip off names
}

#' @rdname monthNumber
#' @export
monthNumber.Date = function(d) {
  as.integer(as.POSIXlt(d)$mon + 1)
}

#' @rdname monthNumber
#' @export
monthNumber.yearmon = function(d) {
  as.integer(as.POSIXlt(d)$mon + 1)
}

# --------------------------------------------------------------

#'
#'  Return a futures month code
#'
#' @param m A month number, month name, Date, or yearmon
#' @return One-character futures month code,
#'    F = Jan, G = Feb, H = March, etc.
#'
#' @export
#'
monthCode = function(m) {
    n = monthNumber(m)
    c("F", "G", "H",
      "J", "K", "M",
      "N", "Q", "U",
      "V", "X", "Z")[n]
}

# --------------------------------------------------------------

# monthName = function(x) ...

# --------------------------------------------------------------

#'
#'  Serial number of a month
#'
#'  Given a date or a year/month combination,
#'  return the number of months since Jan 1, 1970.
#'
#' @param x Method dependent
#' @return A serial integer for the month
#' @export
#'
monthIndex = function(x, ...) UseMethod("monthIndex", x)

#' @export
monthIndex.default = function(x, ...) {
  stop("Cannot convert ", class(x), " to month index")
}

#' @rdname monthIndex
#' @export
monthIndex.Date = function(x) {
  lt = as.POSIXlt(x)
  years = lt$year + 1900 - 1970
  12*years + lt$mon
}

#' @rdname monthIndex
#' @export
monthIndex.POSIXlt = function(x) {
  years = x$year + 1900 - 1970
  12*years + x$mon
}

#' @rdname monthIndex
#' @export
monthIndex.integer = function(year, month) {
  12*(year - 1970) + month - 1
}

#' @rdname monthIndex
#' @export
monthIndex.numeric = function(year, month) {
  12*(year - 1970) + month - 1
}

#' @rdname monthIndex
#' @export
monthIndexToDate = function(mi) {
  month = (mi %% 12) + 1
  year = (mi %/% 12) + 1970
  as.Date(ISOdate(year, month, 1))
}
