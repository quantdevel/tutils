#'
#'  Tau-style warning message
#'
#'  This is like base::warning(), but friendlier.
#'
#' @param ... The message (characters)
#' @param sep Message separator (character)
#' @param call. Passed to base::warning (logical)
#' @param immediate. Passed to base::warning (logical)
#' @returns Returns the warning message as a character string, invisibly,
#'   but mostly called for its side effect
#' @export
#'
tau_warn = function(..., sep = " ", call. = FALSE, immediate. = TRUE) {
  warning(paste(..., sep = sep),
          call. = call., immediate. = immediate. )
}
