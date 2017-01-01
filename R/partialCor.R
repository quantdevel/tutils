#'
#'  Calculate partial correlation
#'
#'  Note that all input vectors must be the same length.
#'
#' @param x A vector
#' @param y A vector
#' @param z A vector
#' @param use Passed to 'use' parameter of cor()
#' @return Partial correlation
#'
#' @export
#'
partialCor <- function(x, y, z, use="complete.obs") {
	rxy = cor(x, y, use=use)
	rxz = cor(x, z, use=use)
	ryz = cor(y, z, use=use)

	return((rxy - rxz*ryz) / sqrt((1 - rxz^2) * (1 - ryz^2)))
}

