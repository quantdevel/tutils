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
