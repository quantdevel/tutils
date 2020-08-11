#'
#'  Convert xts object to tibble
#'
#' @param x An xts object
#' @param pivot_longer If TRUE, pivot columns of \code{x}
#'   into long format
#" @param ... Unused
#'
#' @return Returns a tibble.
#'   If \code{pivot_longer} is TRUE,
#'   the columns are \code{index}, \code{series}, and \code{value},
#'   where values of \code{series} are taken from the column names of \code{x}.
#'   Otherwise, the tibble has the columns of \code{x},
#'   plus an additional column called \code{index}.
#'
#' @importFrom broom tidy
#' @export
#'
tidy.xts = function(x, pivot_longer = TRUE, ...) {
  ret <- tibble::tibble(index = zoo::index(x),
                        tibble::remove_rownames(xts:::as.data.frame.xts(x)) )

  if (pivot_longer) {
    ret <- pivot_longer(ret, cols = c(dplyr::everything(), -index),
                        names_to = "series", values_to = "value")
  }

  return(ret)
}
