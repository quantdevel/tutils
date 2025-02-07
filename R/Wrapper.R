#'
#'  Wrap a value and optional messages
#'
#'  The `wrap` function wraps a value inside a `Wrapper` container,
#'  possibly incorporating wrapped messages, too.
#'
#'  The `unwrap` function extracts the value from the container.
#'  Unless `quiet = TRUE`, it will print the wrapped, associated messages, if any,
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
#' @returns `Wrapper` and `wrap` eturn a list with class `Wrapper`.
#'   `unwrap` returns the value wrapped inside the Wrapper container.
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

# wrap function ----

#'
#'  Wrap an object
#'
#' @seealso [Wrapper] object class
#' @param value The value to be wrapped
#' @param w A Wrapper object whose value will be wrapped
#'   and whose messages will be incorporated into the Wrapper
#' @returns Returns a Wrapper object
#' @export
#'
wrap = function(x, ...) UseMethod("wrap", x)

#' @export
wrap.default = function(value, ...) {
  Wrapper(value = value, ...)
}

#' @export
wrap.Wrapper = function(w, errors = list(), warnings = list(), info = list(),
                        include = list(), ...) {
  Wrapper(w$value,
          errors = c(errors, w$errors),
          warnings = c(warnings, w$warnings),
          info = c(info, w$info),
          include = include,
          ... )
}

# unwrap function ----

#'
#'  Extract the value from a Wrapper object
#'
#' @param x Can be a Wrapper object or anything else
#' @param quiet (boolean)
#' @returns If x is a Wrapper object, its value is returned.
#'   Otherwise, x is returned unchanged.
#' @export
#'
unwrap = function(x, ...) UseMethod("unwrap", x)

#' @rdname unwrap
#' @export
unwrap.default = function(x, ...) x

#' @rdname unwrap
#' @export
unwrap.Wrapper = function(x, quiet = TRUE) {
  if (!quiet) {
    purrr::walk(purrr::compact(x$errors), .f = \(.) catln("Error:", .))
    purrr::walk(purrr::compact(x$warnings), .f = \(.) catln("Warning:", .))
    purrr::walk(purrr::compact(x$info), .f = tutils::catln)
  }

  return(x$value)
}

# Other unwrap() functions ----

#' @export
unwrapErrors = function(x) {
  if (is.Wrapper(x) && length(x$errors) > 0) {
    x$errors
  }
}

#' @export
unwrapWarnings = function(x) {
  if (is.Wrapper(x) && length(x$warnings) > 0) {
    x$warnings
  }
}

#' @export
unwrapInfo = function(x) {
  if (is.Wrapper(x) && length(x$info) > 0) {
    x$info
  }
}
