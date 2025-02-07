#'
#'  Format a number in "financial" format
#'
#' @param x Value to be formatted (numeric)
#' @param prefix Prefix to prepend, typically a currency symbol (character, optional)
#' @param accuracy Accuracy of formatted result;
#'   e.g., 1 for rounding to nearest dollar, 0.01 for rounding to nearest penny
#'   (numeric, optional)
#' @param ... Passed to `financialFmr`
#' @return A string
#' @export
#'
financialFmt = function(x, prefix = "", accuracy = 1) {
    s = scales::comma(abs(x), accuracy = accuracy)
    s = paste0(prefix, s)
    s = ifelse(x >= 0, s, paste0("(", s, ")"))
    return(s)
}

#' @rdname financialFmt
#' @export
dollarFmt = function(x, ...) {
  financialFmt(x, prefix = "$", ...)
}
