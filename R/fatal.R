#'
#' Report or test a fatal condition
#'
#' @param ... Passed to \code{cat}
#' @param cond Boolean expression that must be FALSE (\code{fatalIf})
#'   or must be TRUE (\code{fatalIfNot}), otherwise a \code{stop} error is signalled
#' @param sep Separator, passed to \code{cat} (character)
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
    fatal(...,
          caller = caller %||% as.list(sys.call(-1))[[1]] )
  }
}

#' @rdname fatal
#' @export
fatalIfNot = function(cond, ..., caller = NULL) {
  if (!cond) {
    fatal(...,
          caller = caller %||% as.list(sys.call(-1))[[1]] )
  }
}
