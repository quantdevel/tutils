#
#  Misc. small types
#

# ----------------------------------------------------------

#
#   Side type
#

#' @export
SHORT_SIDE = structure("short", class = "Side")

#' @export
LONG_SIDE = structure("long", class = "Side")

#' @export
Side = function(x) {
  fatalIfNot(all(x %in% c(SHORT_SIDE, LONG_SIDE)), "Invalid Side values")
  structure(x, class = "Side")
}

#' @export
is.Side = function(x) inherits(x, "Side")

#' @export
as.Side = function(x) UseMethod("as.Side", x)

#' @export
as.Side.Side = function(x) x

#' @export
as.Side.numeric = function(x) {
  fatalIfNot(all(x %in% c(-1, +1)), "Cannot convert numeric to Side")
  Side(ifelse(x == +1, LONG_SIDE, SHORT_SIDE))
}

#' @export
as.Side.character = function(x) {
  fatalIfNot(all(x %in% c(SHORT_SIDE, LONG_SIDE)), "Cannot convert character to Side")
  Side(x)
}

#' @export
as.integer.Side = function(x) {
  ifelse(x == "long", +1L, -1L)
}

# ----------------------------------------------------------

#
#   Polarity type
#

#' @export
SHORT_POLARITY = structure(-1L, class = "Polarity")

#' @export
LONG_POLARITY = structure(+1L, class = "Polarity")

#' @export
Polarity = function(x) {
  fatalIfNot(all(x %in% c(-1, +1)), "Invalid Polarity")
  structure(as.integer(x), class = "Polarity")
}

#' @export
is.Polarity = function(x) inherits(x, "Polarity")

#' @export
as.Polarity = function(x) UseMethod("as.Polarity", x)

#' @export
as.Polarity.Polarity = function(x) x

#' @export
as.Polarity.numeric = function(x) {
  fatalIfNot(all(x %in% c(-1, +1)), "Cannot convert numeric to Polarity")
  Polarity(x)
}

#' @export
as.Polarity.character = function(x) {
  fatalIfNot(all(x %in% c("long", "short")), "Cannot convert character to Polarity")
  Polarity(ifelse(x == "long", +1L, -1L))
}

#' @export
as.Polarity.Side = function(x) {
  Polarity(ifelse(x == "long", +1L, -1L))
}
