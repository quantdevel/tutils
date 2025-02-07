#'
#'  Wrap a data frame to create an object instance
#'
#' @param df Data frame to be wrapped (data.frame)
#' @param className Class of new instance (character)
#' @param required Names of required columns, NULL if none (character, optional)
#' @param optional Name of optional columns, NULL if none (character, optional)
#' @param cleanse If TRUE, remove all but the required and optional columns (logical)
#' @returns The input data frame, converted to a tibble, possibly
#'   cleansed of extraneous columns (`cleanse = TRUE`),
#'   and with a class attribute given by `className`.
#' @examples
#' df <- data.frame(Pi = pi, E = exp(1), Foo = "fum")
#' wrapObject(df, "Numbers")
#' wrapObject(df, "Numbers", required = c("Pi"), cleanse = TRUE)
#' wrapObject(df, "Numbers", required = c("Pi"), optional = c("E"))
#' wrapObject(df, "Numbers", required = c("Pi"), optional = c("E"), cleanse = TRUE)
#'
#' @export
#'
wrapObject = function(df, className, required = NULL, optional = NULL, cleanse = FALSE) {
  decl(df, is.data.frame)
  decl(className, is.character)
  decl(required, is.null %or% is.character)
  decl(optional, is.null %or% is.character)
  decl(cleanse, is.logical)

  fatalIfNot(all(required %in% colnames(df)),
             "Missing columns required for", className, "object" )

  if (cleanse) {
    df <- df[ , colnames(df) %in% c(required, optional), drop = FALSE]
  }

  df <- (df
         |> tibble::as_tibble() )

  structure(df, class = c(className, class(df)))
}
