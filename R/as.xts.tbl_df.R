#'
#'  Convert a tibble to xts
#'
#' @param x A tibble to be converted to xts
#' @param index (character)
#'
#' @importFrom xts as.xts
#' @export
#'
as.xts.tbl_df = function(x, index = NULL) {
  ensure(index, is.null(.) || is.character(.))

  if (is.null(index)) {
    date_cand <- intersect(colnames(x),
                           c("index", "Index", "date", "Date") )
    if (length(date_cand) > 0) {
      index <- date_cand[[1]]
    } else {
      fatal("as.xts: Cannot find index or date column in data frame")
    }

  } else {
    fatalIfNot(index %in% colnames(x),
               "as.xts: No such column in data frame:", index)
  }

  pos <- which(colnames(x) == index)
  xts::xts(x[ , -pos], x[ , pos, drop = TRUE])
}
