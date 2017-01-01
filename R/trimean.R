#'
#'  Trimean - robust location statistic
#'
#'  The trimean is defined as (Q1 + 2*Q2 + Q3) / 4.
#'
#' @param x Vector of numeric values
#' @param na.rm If TRUE, remove NA values from x
#'
#' @return Trimean value
#'
#' @export
#'
trimean = function(x, na.rm=FALSE) {
  q = quantile(x, probs=c(0.25, 0.50, 0.75),
               na.rm=na.rm, names=FALSE)
  (q[1] + 2*q[2] + q[3]) / 4
}
