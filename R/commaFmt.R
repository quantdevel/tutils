#'
#'  Format a number with embedded commas
#'
#' @param x A numeric value
#' @param Number of places to right of decimal point
#' @return Formatted number (character)
#' @export
#'
commaFmt <- function(x, digits=0) {
	prettyNum(round(as.numeric(x),digits=digits), big.mark=",")
}

