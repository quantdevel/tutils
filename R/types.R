#
#  Misc. small types
#

# Side type ----------------------------------------------------------

#' @export
SHORT_SIDE = structure("short", class = "Side")

#' @export
LONG_SIDE = structure("long", class = "Side")

#' @export
NA_SIDE = structure(NA_character_, class = "Side")

#' @title Side type
#' @description
#' The `Side` type indicates a *long* or *short* position
#' by using the strings "long" and "short".
#'
#' - "long" indicates position that is owned
#' - "short" indicates a position sold short
#'
#' @param x A character vector with values "long" and "short".
#' @seealso [Polarity] type, [Tone] type
#' @export
Side = function(x) {
  fatalIfNot(all(x %in% c(SHORT_SIDE, LONG_SIDE, NA)), "Invalid Side values")
  structure(x, class = "Side")
}

#' @rdname Side
#' @export
is.Side = function(x) inherits(x, "Side")

#' @rdname Side
#' @export
as.Side = function(x) UseMethod("as.Side", x)

#' @export
as.Side.Side = function(x) x

#' @export
as.Side.numeric = function(x) {
  fatalIfNot(all(x %in% c(-1, +1, NA)), "Invalid numeric Side values")

  (dplyr::case_when(is.na(x) ~ NA_character_,
                    x == +1 ~ "long",
                    x == -1 ~ "short")
    |> Side() )
}

#' @export
as.Side.character = function(x) {
  Side(x)
}

#' @export
as.Side.Polarity = function(x) {
  as.Side.numeric(x)

  # (dplyr::case_when(is.na(x) ~ NA_character_,
  #                   x == +1L ~ "long",
  #                   x == -1L ~ "short")
  #  |> Side() )
}

#' @export
as.Side.Tone = function(x) {
  Side(ifelse(x == BULL_MKT, LONG_SIDE, SHORT_SIDE))
}

#' @export
as.integer.Side = function(x) {
  dplyr::case_when(is.na(x) ~ NA_integer_,
                   x == "long" ~ +1L,
                   x == "short" ~ -1L )
}

# Polarity type ----------------------------------------------------------

#' @export
SHORT_POLARITY = structure(-1L, class = "Polarity")

#' @export
LONG_POLARITY = structure(+1L, class = "Polarity")

#' @title Polarity type
#' @description
#' The `Polarity` type indicates a *long* or *short* position
#' by using +1 and -1.
#'
#' - +1 indicates a long position.
#' - -1 indicates a short position
#'
#' Note that this indicates only the polarity (long vs. short),
#' not the size of the position.
#'
#' @param x A vector integer values +1 or -1
#' @seealso [Side] type, [Tone] type
#' @export
Polarity = function(x) {
  fatalIfNot(all(x %in% c(-1, +1, NA)), "Invalid Polarity values")
  structure(as.integer(x), class = "Polarity")
}

#' @rdname Polarity
#' @export
is.Polarity = function(x) inherits(x, "Polarity")

#' @rdname Polarity
#' @export
as.Polarity = function(x) UseMethod("as.Polarity", x)

#' @export
as.Polarity.Polarity = function(x) x

#' @export
as.Polarity.numeric = function(x) {
  Polarity(x)

  # fatalIfNot(all(x %in% c(-1, +1, NA)),
  #            "Numeric Polarity values must be +1, -1, or NA" )
  # Polarity(x)
}

#' @export
as.Polarity.character = function(x) {
  fatalIfNot(all(x %in% c("long", "short", NA)),
             "Character Polarity values must be 'long', 'short', or NA" )

  (dplyr::case_when(is.na(x) ~ NA_integer_,
                    x == "long" ~ +1L,
                    x == "short" ~ -1L )
    |> as.Polarity() )
}

#' @export
as.Polarity.Side = function(x) {
  as.Polarity.character(x)

  # (dplyr::case_when(is.na(x) ~ NA_integer_,
  #                   x == "long" ~ +1L,
  #                   x == "short" ~ -1L )
  #  |> as.Polarity() )
}

#' @export
as.Polarity.Tone = function(x) {
  Polarity(ifelse(x == BULL_MKT, +1L, -1L))
}

# Market Tone ----------------------------------------------------------

#
#  Tone can be bullish or bearish
#

#' @export
BEAR_MKT = "bear"

#' @export
BULL_MKT = "bull"

TONE_LEVELS = c(BEAR_MKT, BULL_MKT)

#' @title Tone type
#' @description A `Tone` type indicates if a market is
#' generally trending *upward* ("bull") or *downward* ("bear").
#' @param x A character vector of the values "bull" and "bear"
#' @seealso [Side] type, [Polarity] type
#' @export
Tone = function(x) {
  fatalIfNot(all(x %in% TONE_LEVELS),
             "Invalid Tone value" )
  structure(x, class = "Tone")
}

#' @rdname Tone
#' @export
is.Tone = function(x) inherits(x, "Tone")

#' @rdname Tone
#' @export
as.Tone = function(x) UseMethod("as.Tone", x)

#' @export
as.Tone.Tone = function(x) x

#' @export
as.Tone.numeric = function(x) {
  fatalIfNot(all(x %in% c(-1, +1)), "Cannot convert numeric to Tone")
  Tone(ifelse(x == +1, BULL_MKT, BEAR_MKT))
}

#' @export
as.Tone.character = function(x) {
  fatalIfNot(all(x %in% TONE_LEVELS),
             "Cannot convert character to Tone")
  Tone(x)
}

#' @export
as.Tone.Polarity = function(x) {
  Tone(ifelse(x == +1, BULL_MKT, BEAR_MKT))
}

#' @export
as.Tone.Side = function(x) {
  Tone(ifelse(x == "long", BULL_MKT, BEAR_MKT))
}

#' @export
as.integer.Tone = function(x) {
  ifelse(x == BULL_MKT, +1L, -1L)
}
