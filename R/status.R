#
# Status and alerts management
#

status_file_path = function(name) {
  file.path("/tau/status",
            if (endsWith(name, ".json")) name else paste0(name, ".json") )
}

#'
#' @title Post process status
#' @description
#' Any Tau process can post a status, indicating all-OK or problems.
#' Other processes can view the posted status.
#'
#' Typically, posting status is the final act before exiting.
#'
#' If the status is set with `alert = TRUE`, then the end user is
#' notified of the status update.
#' @param name Name of the posting process (character)
#' @param status Status indicator, such as "ok", "error", or "failed".
#' @param message Optional useful message.
#' @param url Optional URL where the user can find more information.
#' @export
#' @examples
#' # Signal that my_process ended successfully
#' post_status("my_process", status = "ok")
#'
#' # Signal that other_process ended on an error
#' post_status("other_process", status = "error")
#'
#' # Same, and notify user
#' post_status("other_process", status = "error", alert = TRUE)
#'
post_status = function(name, status, alert = FALSE,
                       message = NA_character_, url = NA_character_ ) {
  decl(name, is.character)
  decl(status, is.character)
  decl(alert, is.logical)
  decl(message, is.character)
  decl(url, is.character)

  timestamp <- Sys.time()
  fpath <- status_file_path(name)

  (data.frame(name = name,
              status = status,
              alert = alert,
              timestamp = format(timestamp),
              message = message,
              url = url )
    |> jsonlite::toJSON(pretty = TRUE, na = "string")
    |> writeLines(con = fpath) )

  invisible(NULL)
}

#'
#' @title Post an alert
#' @description
#' Post the status of a process, and alert the user to the status change.
#'
#' This is a convenience function that calls [post_status] with
#' `alert=TRUE`.
#' Parameters are same as to [post_status].
#'
#' @param name (character)
#' @param status (character)
#' @param message (character or list of character))
#' @param url (character)
#' @export
#' @examples
#' # Post an error status and ask for notification of user
#' post_alert("other-process", status = "error")
#'
post_alert = function(name, status = "error",
                      message = NA_character_, url = NA_character_) {
  post_status(name = name,
              status = status,
              alert = TRUE,
              message = message,
              url = url )
}

#'
#' @title Read the status of a process
#' @param name The name of the process
#' @returns Returns a list
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

#'
#' @title Read all current status files
#' @description
#' A status file is *current* if its timestamp
#' is the current settlement date or later
#' @returns Returns a data frame of status information.
#' Returns an empty data frame if there are not current
#' status reports.
#' @export
#'
read_current_status = function() {
  midnight <- as.POSIXct(taudata::settleDate())

  ("/tau/status"
    |> list.files(pattern = ".json")
    |> purrr::map(read_status)
    |> purrr::keep(~ .$timestamp >= midnight)
    |> purrr::list_rbind() )
}

#' @export
read_current_alerts = function() {
  (read_current_status()
   |> dplyr::filter(alert) )
}
