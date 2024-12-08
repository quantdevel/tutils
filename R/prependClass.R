#'
#'  Prepend a class attribute to existing class attribute
#'
#' @param x An object
#' @param className Class name to prepend to existing class attribute of x
#' @return Returns x with updated class attribute
#' @examples
#' prependClass(mtcars, "Foo") |> class()
#' @export
#'
prependClass = function(x, className) {
  decl(className, is.character)

  structure(x, class = c(className, class(x)))
}
