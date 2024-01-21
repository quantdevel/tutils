#'
#' Default value for `NULL`
#'
#' This infix function makes it easy to replace `NULL`s with a default
#' value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @export
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
#'
`%||%` = function(x, y) {
  if (is.null(x)) y else x
}

#'
#'  Combine two predicates
#'
#'  Given two predicates, this operator combines them
#'  into one, new predicate. The new predicate calls
#'  the given predicates, applying "||" to the results.
#'
#'  Typical usage within the Tau software would be
#'  \code{decl(x, is.null \%or\% is.numeric)}.
#'
#' @param p1 A predicate of one argument
#' @param p2 A predicate of one argument
#' @return Returns a \emph{function}, not a value. (Be careful.)
#' @export
#' @name op-disjunction-functional
#' @examples
#' (is.null %or% is.numeric)(NULL)
#' (is.null %or% is.numeric)(pi)
#' (is.null %or% is.numeric)("foo")
#'
`%or%` = function(p1, p2) function(x) p1(x) || p2(x)
