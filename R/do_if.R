#'
#' Conditionally apply expressions to a data object
#' 
#' @param .data Input data
#' @param condition A logical value to determine whether to use .then or .else
#' @param .then Formula or function to apply to input data when condition is TRUE
#' @param .else Formula or function to apply to input data when condition is FALSE;
#'   if NULL and condition is FALSE, data is left unchanged
#' @return Output of appropriate .then/.else call
#' @export
#'
do_if <- function(.data, condition, .then, .else = NULL) {
  if (condition) {
    call <- rlang::as_closure(.then)
  } else if (!is.null(.else)) {
    call <- rlang::as_closure(.else)
  } else {
    return(.data)
  }
  do.call(call, list(.data))
}
