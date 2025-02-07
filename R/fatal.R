#'
#' Report or test a fatal condition
#'
#' @param ... Passed to `cat`
#' @param cond Boolean expression that must be FALSE (`fatalIf`)
#'   or must be TRUE (`fatalIfNot`), otherwise a `stop` error is signalled
#' @param sep Separator, passed to `cat` (character)
#' @param caller Name of calling function, defaults to name in caller's stack frame (character)
#' @export
#'
fatal = function(..., sep = " ", caller = NULL) {
  msg <- paste(..., sep = sep)

  caller <- caller %||%
    {
      calls <- sys.call(-1)
      if (!is.null(calls)) {
        as.list(calls[[1]])
      }
    }
  if (!is.null(caller)) {
    msg <- paste0("[", caller, "] ", msg)
  }

  stop(msg, call. = FALSE)
}

#' @rdname fatal
#' @export
fatalIf = function(cond, ..., caller = NULL) {
  if (cond) {
    fatal(..., caller = caller)
  }
}

#' @rdname fatal
#' @export
fatalIfNot = function(cond, ..., caller = NULL) {
  if (!cond) {
    fatal(..., caller = caller)
  }
}
