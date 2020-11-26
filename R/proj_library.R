#'
#'  Attach the files in project's R directory
#'
#'  This will source all *.R files in the R subdirectory
#'  of the current project.
#'  It loads their contents into a new environment,
#'  and attaches that environment at the top of the search
#'  path (but underneith the global environment).
#'
#' @param files Vector or list of files to source;
#'   NULL means all *.R files (character)
#' @param ... Passed to \code{source}
#' @return Nothing
#' @seealso \link{proj_source} for sourcing project files
#'  into the global environment,
#'  \link{detach} for removing the new environment
#'  from the search path.
#' @export
#'
proj_library = function(files = NULL, ...) {
  ensure(files, is.null(.) || is.character(.))

  dir = here::here("R")
  if (is.null(files)) {
    files = list.files(path = dir, pattern = ".R$")
  }

  env = new.env(parent = as.environment(2))

  for (file in files) {
    source(file.path(dir, file),
           local = env, ...)
  }

  attach(env, pos = 2L, name = "proj_library")
}
