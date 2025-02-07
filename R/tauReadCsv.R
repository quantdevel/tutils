#'
#'  Tau wrapper for read.csv()
#'
#'  This just calls [read.csv], but it provides better defaults.
#'
#' @param file A connection or filename for the CSV file
#' @param header Passed to read.csv(), defaults to TRUE
#' @param stringsAsFactors Passed to read.csv(), defaults to FALSE
#' @param comment.char Passed to read.csv, defaults to "#"
#' @param ... Any other arguments for read.csv()
#' @return The data frame from the CSV file
#'
#' @export
#'
tauReadCsv = function(file, header=TRUE, stringsAsFactors=FALSE,
                      comment.char="#", ... ) {
  read.csv(file, header=header, stringsAsFactors=stringsAsFactors,
           comment.char=comment.char, ... )
}
