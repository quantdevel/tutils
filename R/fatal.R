#' @export
fatal = function(..., sep = " ", caller = NULL) {
  caller <- caller %||% as.list(sys.call(-1))[[1]]
  msg <- paste0("[", caller, "] ", paste(..., sep = sep))
  stop(msg, call. = FALSE)
}

#' @export
fatalIf = function(cond, ..., caller = NULL) {
  if (cond) {
    fatal(...,
          caller = caller %||% as.list(sys.call(-1))[[1]] )
  }
}

#' @export
fatalIfNot = function(cond, ..., caller = NULL) {
  if (!cond) {
    fatal(...,
          caller = caller %||% as.list(sys.call(-1))[[1]] )
  }
}
