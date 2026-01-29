#'
#' @title Log a warning message
#' @description
#' This is a wrapper for logger::log_warn,
#' letting the caller set the separator.
#' @param ... The message elements (characters)
#' @param sep Message separator (character)
#' @param call. Obsolete, not used.
#' @param immediate. Obsolete, not used.
#' @returns Invisible list of logger objects,
#' but mostly called for it's side effect.
#' @export
#'
tau_warn = function(..., sep = " ", call. = FALSE, immediate. = TRUE) {
  logger::log_warn(paste(..., sep = sep))
}
