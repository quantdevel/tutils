#'
#'  Source files in project's R directory
#'
#'  This will source all *.R files in the R subdirectory
#'  of the current project.
#'  Their contents are loaded into the global environment.
#'
#' @param files Vector or list of files to source;
#'   NULL means all *.R files (character)
#' @param ... Passed to \code{source}
#' @return Nothing
#' @seealso \link{proj_library} for sourcing project files
#'  into a new, attached environment.
#' @export
#'
proj_source = function(files = NULL, ...) {
  ensure(files, is.null(.) || is.character(.))

  dir = here::here("R")
  if (is.null(files)) {
    files = list.files(path = dir, pattern = ".R$")
  }

  for (file in files) {
    source(file.path(dir, file), local = FALSE, ...)
  }
}
