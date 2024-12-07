#'
#'  On or off?
#'
#' @param b A boolean value
#' @return Returns "on" if b is a single, defined boolean value that is TRUE;
#'    "off" otherwise
#' @export
#'
on_off = function(b) {
  if (base::isTRUE(b)) "on" else "off"
}
