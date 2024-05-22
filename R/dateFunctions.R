#'
#'  Convert a yyyymmdd value to a Date
#'
#' @param yyyymmdd A string or integer of year, month, date
#' @return A Date object
#'
#' @export
#'
yyyymmddToDate = function(yyyymmdd) {
  decl(yyyymmdd, is.character %or% is.numeric)

  yyyymmdd = as.integer(yyyymmdd)
  as.Date(ISOdate((yyyymmdd %/% 10000),
                  (yyyymmdd %/% 100) %% 100,
                  (yyyymmdd %%  100) ))
}
