#'
#'  Wrap a value and optional messages
#'
#'  The \code{wrap} function wraps a value inside a special container object,
#'  possibly wrapping some messages.
#'
#'  The \code{unwrap} function extracts the value from the container.
#'  Unless \code{quiet = TRUE}, it will print the wrapped, associated messages, if any,
#'  before returning that extracted value.
#'
#' @param x A value to be wrapped
#' @param messages List of associated messages (list, optional)
#' @param element (character)
#' @param quiet If TRUE, print the enclosed messages, if any, before
#'   extracting the enclosed value (logical)
#' @returns Return a list with class \code{Wrapper}
#' @export
#' @examples
#' w <- wrap(pi, messages = "Help me!")
#'
#' # Extract the wrapped value
#' unwrap(w)
#'
#' # Extract the wrapped value and print the wrapped messages
#' unwrap(w, quiet = FALSE)
#'
Wrapper = function(value, messages, ...) {
  (append(list(value = value, messages = messages),
          list(...) )
   |> structure(class = "Wrapper") )
}

#' @rdname Wrapper
#' @export
is.Wrapper = function(x) inherits(x, "Wrapper")

#' @rdname Wrapper
#' @export
wrap = function(value, messages = list(), ...) {
  Wrapper(value = value, messages = messages, ...)
}

#' @rdname Wrapper
#' @export
unwrap = function(x, ...) UseMethod("unwrap", x)

#' @rdname Wrapper
#' @export
unwrap.default = function(x, ...) x

#' @rdname Wrapper
#' @export
unwrap.Wrapper = function(x, element = NULL, quiet = TRUE) {
  if (!quiet && length(x$messages) > 0) {
    purrr::walk(x$messages, .f = tutils::catln)
  }
  if (is.null(element)) {
    x$value
  } else {
    if (element %in% names(x)) {
      x[[element]]
    } else {
      tutils::tau_warn("Wrapper does not contain element '", element,
                       "', using NULL instead", sep = "" )
      NULL
    }
  }
}
