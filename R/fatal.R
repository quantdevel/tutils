#' @export
fatal = function(..., sep = " ") {
  msg = paste(..., sep = sep)
  stop(msg, call. = FALSE)
}

#' @export
fatalIf = function(cond, ...) {
  if (cond) {
    fatal(...)
  }
}

#' @export
fatalIfNot = function(cond, ...) {
  if (!cond) {
    fatal(...)
  }
}
