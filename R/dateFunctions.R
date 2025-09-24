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

#'
#' Format a date concisely
#'
#' Format a date as "MMM, DD/MM", for example "Wed, 09/24".
#'
#' This format is useful in contexts where space is tight, readability is valuable,
#' and the year is understood (or not important).
#'
#' @param d Date object
#' @returns Returns the formatted date (character)
#' @export
#'
ddd_mm_dd = function(d) {
    format(d, "%a, %m/%d")
}
