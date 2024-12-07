#'
#'  Yes or no?
#'
#' @param b A boolean value
#' @return Returns "yes" if b is a single, defined boolean value that is TRUE;
#'    "no" otherwise
#' @export
#'
yes_no = function(b) {
  if (base::isTRUE(b)) "yes" else "no"
}
