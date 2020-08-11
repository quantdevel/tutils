#' @export
messageIf = function(cond, ..., sep = " ") {
  if (cond) {
    msg = paste(..., sep = sep)
    message(msg)
  }
}

#' @export
messageIfNot = function(cond, ..., sep = " ") {
  if (!cond) {
    msg = paste(..., sep = sep)
    message(msg)
  }
}
