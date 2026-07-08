#
# Status and alerts management
#

status_file_path = function(name) {
  file.path("/tau/status",
            if (endsWith(name, ".json")) name else paste0(name, ".json") )
}

#'
#' @title Set process status
#' @export
#'
post_status = function(name, status, alert = FALSE, messages = list(), url = NULL) {
  decl(name, is.character)
  decl(status, is.character)
  decl(alert, is.logical)
  decl(messages, is.list)
  decl(url, is.null %or% is.character)

  timestamp <- Sys.time()
  fpath <- status_file_path(name)

  (list(name = name,
        status = status,
        alert = alert,
        timestamp = format(timestamp),
        messages = messages,
        url = url )
    |> jsonlite::toJSON(pretty = TRUE)
    |> writeLines(con = fpath) )
}

#'
#' @title Post an alert
#' @export
#'
post_alert = function(name, status = "error", messages = list(), url = NULL) {
  post_status(name = name,
              status = status,
              alert = TRUE,
              messages = messages,
              url = url )
}

#'
#' @title Read process status
#' @export
#'
read_status = function(name) {
  decl(name, is.character)

  status <- (status_file_path(name)
             |> readLines()
             |> paste(collapse = "\n")
             |> jsonlite::fromJSON() )
  status$timestamp <- as.POSIXct(status$timestamp)
  return(status)
}

## TODO: read_active_status = status whose timestamp is yesterday

## TODO: read_active_alerts = status whose timestamp is yesterday and alert == TRUE
