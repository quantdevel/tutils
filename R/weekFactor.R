#'
#' Create a week-of-the-year factor from zoo or xts object
#' 
#' @param z A zoo or xts object
#' @return A factor with levels 1, ..., 53.
#' @export
#'
weekFactor <- function(z) {
	ydays <- as.POSIXlt(index(z))$yday
	factor(
		(ydays %/% 7) + 1,
		levels=1:53		# There are slightly more than 52 weeks in 365 days
		)
}
