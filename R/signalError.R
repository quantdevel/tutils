#'
#'  Signal an error condition
#'
#'  Signal an error condition. Possibly log the error, too.
#'
#' @param ... Character strings that will be pasted together into an error message
#' @param sep Separator for strings in error message (character)
#' @param log If TRUE, log the error
#' @seealso The message is logged via [logger::log_error()].
#' @returns Returns nothing. Called for its effect.
#' @export
#'
signalError = function(..., sep = " ", log = TRUE) {
  msg <- paste(..., sep = sep)
  if (log) {
    logger::log_error(msg)
  }
  signalCondition(simpleError(msg))
}

#' @rdname signalError
#' @export
signalErrorIf = function(b, ...) {
  if (b) {
    signalError(...)
  }
}

#' @rdname signalError
#' @export
signalErrorIfNot = function(b, ...) {
  if (!b) {
    signalError(...)
  }
}
