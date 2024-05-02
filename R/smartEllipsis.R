#'
#'  Employ an ellipsis, if necessary
#'
#' @param strings Vector of strings to be joined (character)
#' @param max Number of strings before ellipsis (integer)
#' @param tail Number of strings after the ellipsis (integer, optional)
#' @param empty Returned if \code{strings} is empty (character)
#' @returns All the \code{strings} pasted together, separated by commas,
#'   up to a maximum of \code{max} strings;
#'   if \code{strings} has more than \code{max} elements,
#'   the extra elements are replaced by an ellipsis.
#'   If \code{strings} is empty, then \code{empty} is returned.
#' @export
#' @examples
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 4)
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 3)
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 3, tail = 0)
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 2)
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 2, tail = 0)
#' smartEllipsis(character(0), 3)
#'
smartEllipsis = function(strings, max = 2, tail = 1, empty = "None") {
  decl(strings, is.character)
  decl(max, is.numeric)

  if (length(strings) == 0) {
    return(empty)
  }
  if (length(strings) <= max + tail) {
    paste(strings, collapse = ", ")
  } else {
    paste(c(head(strings, max),
            "...",
            if (tail > 0) {
              tail(strings, tail)
            } ),
          collapse = ", " )
  }
}

