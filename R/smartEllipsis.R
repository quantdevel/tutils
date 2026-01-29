#'
#' @title Join a list of strings, using ellipsis if necessary
#' @description
#' This joins a vector of strings into one string
#' with the elements separated by commas.
#' If there are too many inputs, however,
#' will use an ellipsis to shorten the output.
#'
#' @param strings Vector of strings to be joined (character)
#' @param max Number of strings before ellipsis (integer)
#' @param tail Number of strings after the ellipsis (integer, optional)
#' @param empty Returned if `strings` is empty (character)
#' @returns All the `strings` pasted together, separated by commas,
#'   up to a maximum of `max` strings;
#'   if `strings` has more than `max` elements,
#'   the extra elements are replaced by an ellipsis.
#'   If `strings` is empty, then `empty` is returned.
#' @export
#' @examples
#' # This returns "tom, dick, harry, peter"
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 4)
#'
#' # This also ret urns "tom, dick, harry, peter"
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 3)
#'
#' # This returns "tom, dick, harry, ..."
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 3, tail = 0)
#'
#' # This returns "tom, dick, ..., peter"
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 2)
#'
#' # This returns "tom, dick, ..."
#' smartEllipsis(c("tom", "dick", "harry", "peter"), 2, tail = 0)
#'
#' # This returns "None"
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

