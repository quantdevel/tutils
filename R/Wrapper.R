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
#' @param errors List of error messages (list, optional)
#' @param warnings List of warning messages (list, optional)
#' @param info List of informational messages (list, optional)
#' @param element (character)
#' @param quiet If TRUE, print the enclosed messages, if any, before
#'   extracting the enclosed value (logical)
#' @returns Return a list with class \code{Wrapper}
#' @export
#' @examples
#' w <- wrap(pi, error = "Help me!", warnings = "Just chillin'")
#'
#' # Extract the wrapped value
#' unwrap(w)
#'
#' # Extract the wrapped value and print the wrapped messages
#' unwrap(w, quiet = FALSE)
#'
Wrapper = function(value, errors = list, warnings = list(), info = list(), ...) {
  (append(list(value = value,
               errors = errors,
               warnings = warnings,
               info = info),
          list(...) )
   |> structure(class = "Wrapper") )
}

#' @rdname Wrapper
#' @export
is.Wrapper = function(x) inherits(x, "Wrapper")

#' @rdname Wrapper
#' @export
wrap = function(value, ...) {
  Wrapper(value = value, ...)
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
  if (!quiet) {
    purrr::walk(x$errors, .f = \(.) catln("Error:", .))
    purrr::walk(x$warnings, .f = \(.) catln("Warning:", .))
    purrr::walk(x$info, .f = tutils::catln)
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
