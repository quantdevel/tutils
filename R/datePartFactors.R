#'
#' Create day-of-year factor from zoo or xts object
#'
#' Given an xts/zoo object with a Date index,
#'	return a parallel factor whose levels are the
#'  calendar day of each Date
#'  
#' @param z A zoo or xts object
#' @return Factor with 366 levels
#' @export
#'
calendarDayFactor = function(z) {
    decl(z, zoo::is.zoo)

	ydays <- sapply(index(z), function(x) as.POSIXlt(x)$yday)
    factor(
        ydays,
        levels=0:365)
}

#'
#'  Create month-of-year factor from zoo or xts object
#'  
#' @param z A zoo or xts object
#' @return A factor
#' @export
#'
monthFactor = function(z) {
  decl(z, zoo::is.zoo)
  
	factor(
		months(index(z), abbreviate=TRUE),
		levels=c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec") )
}

#'
#'  Create quarter-of-year factor from zoo or xts object
#'  
#'  Returns a factor with four levels (Q1, Q2, Q3, Q4),
#'  taken from index of the parameter.
#'  
#' @param z A zoo or xts object
#' @return A factor
#' @export
#'
quarterFactor = function(z) {
    decl(z, zoo::is.zoo)
  
    factor(
        quarters(index(z)),
        levels=c("Q1", "Q2", "Q3", "Q4") )
}
