#'
#'  Attach the files in project's R directory
#'
#'  This will source all *.R files in the R subdirectory
#'  of the current project.
#'  It loads their contents into a new environment,
#'  and attaches that environment at the top of the search
#'  path (but underneith the global environment).
#'
#'  If calls to library() or require() are needed, all such calls must
#'  be collected into a special file called "packages.R"
#'  (and that file should contain only calls to library() and/or require()).
#'  Otherwise, the packages will be loaded into the wrong
#'  place on the search list.
#'
#' @param dir Path of `R` directory;
#'   NULL means use the `R` directory of the enclosing
#'   RStudio project (character)
#' @param ... Passed to `source`
#' @return Nothing
#' @seealso [proj_source] for sourcing project files
#'  into the global environment,
#'  [detach] for removing the new environment
#'  from the search path.
#' @export
#'
proj_library = function(dir = NULL, ...) {
  decl(dir, is.null %or% is.character)

  ENV_NAME = "proj_library"

  if (is.null(dir)) {
    dir = here::here("R")
  }
  files = list.files(path = dir, pattern = ".R$")

  if (ENV_NAME %in% search()) {
    detach(ENV_NAME, character.only = TRUE)
  }

  if ("packages.R" %in% files) {
    source(file.path(dir, "packages.R"), local = FALSE)
    files = setdiff(files, "packages.R")
  }

  env = attach(NULL, name = ENV_NAME)

  for (file in files) {
    sys.source(file = file.path(dir, file),
               envir = env )
  }
}
