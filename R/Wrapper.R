#'
#'  Wrap a value and optional messages
#'
#'  The \code{wrap} function wraps a value inside a \code{Wrapper} container,
#'  possibly incorporating wrapped messages, too.
#'
#'  The \code{unwrap} function extracts the value from the container.
#'  Unless \code{quiet = TRUE}, it will print the wrapped, associated messages, if any,
#'  before returning that extracted value (so the message are not ignored).
#'
#'  As a convenience to the caller, the errors, warnings, and info
#'  may be supplied as character vectors (or even NULL), but they will be coerced to lists
#'  and stored as lists.
#'
#' @param x A value to be wrapped
#' @param errors List of error messages (list, optional)
#' @param warnings List of warning messages (list, optional)
#' @param info List of informational messages (list, optional)
#' @param include A list of already-wrapped objects, who's
#'   errors, warnings, and infos should be included in this wrapper
#' @param quiet If TRUE, print the enclosed messages, if any, before
#'   extracting the enclosed value (logical)
#' @returns \code{Wrapper} and \code{wrap} eturn a list with class \code{Wrapper}.
#'   \code{unwrap} returns the value wrapped inside the Wrapper container.
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
#' # Piggy back the error messages
#' wrap(pi, error = "Pi error", include = list( wrap("foo", error = "Foo error") ))
#'
Wrapper = function(value, errors = list(), warnings = list(), info = list(),
                   include = list(), ...) {
  errors <- as.list(errors)       # in case 'errors' is.character or NULL
  warnings <- as.list(warnings)   # ditto
  info <- as.list(info)           # ditto

  # Incorporate the other included messages
  errors <- c(errors, unlist(purrr::map(include, "errors")))
  warnings <- c(warnings, unlist(purrr::map(include, "warnings")))
  info <- c(info, unlist(purrr::map(include, "info")))

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
unwrap.Wrapper = function(x, quiet = TRUE) {
  if (!quiet) {
    purrr::walk(x$errors, .f = \(.) catln("Error:", .))
    purrr::walk(x$warnings, .f = \(.) catln("Warning:", .))
    purrr::walk(x$info, .f = tutils::catln)
  }

  return(x$value)
}
