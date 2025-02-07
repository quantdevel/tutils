#'
#'  Greatest common divisor (GCD)
#'
#'  This is used by the Intermarket Spread objects
#'  to find the largest, common contract size.
#'
#' @param a An integer
#' @param b Another integer
#' @return The greatest common divisor of `a` and `b`
#' @seealso Algorithm is descibed on
#'   [Wikipedia]( https://en.wikipedia.org/wiki/Euclidean_algorithm)
#' @export
#'
gcd = function(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
