#'
#'  Format a number in "financial" format
#'
#' @param x Value to be formatted
#' @param digits Round to this number of digitis, default 2
#'
#' @return A string
#'
#' @export
#'
financialFmt = function(x, digits=2) {
    x = round(x, digits)
    s = scales::comma(abs(x))
    s = ifelse(x >= 0, s, paste0("(", s, ")"))
    return(s)
}
