#
# API for posting events and alerts
#

POSTS_DIR = "/tau/posts"

#' @export
EVENTS_FILE = file.path(POSTS_DIR, "events.jsonl")

# Local functions ----

loadPostsFile = function(fpath) {
  con <- file(fpath, open = "r")

  make_tibble = function(lst) {
    tibble::tibble(
      Origin = lst$Origin,
      Timestamp = as.POSIXct(lst$Timestamp),
      EventType = lst$EventType,
      Payload = list(lst$Payload),
      Alert = lst$Alert
    )
  }

  lines <- (readLines(con)
            |> purrr::map(jsonlite::fromJSON)
            |> purrr::map(make_tibble)
            |> purrr::list_rbind() )

  close(con)
  return(lines)
}

# Event handling ----

#'
#'  Write one event to the event file
#'
#' @param origin (character)
#' @param event_type (character)
#' @param payload An R object which can be serialized into JSON format
#' @param alert If TRUE, this event will be brought to the attention
#'   of the system user
#' @export
#'
postEvent = function(origin, event_type, payload, alert = FALSE) {
  decl(origin, is.character)
  decl(event_type, is.character)
  decl(alert, is.logical)

  timestamp <- Sys.time()
  con <- file(EVENTS_FILE, open = "a")

  (list(Origin = origin,
        Timestamp = timestamp,
        EventType = event_type,
        Payload = payload,
        Alert = alert )
    |> jsonlite::toJSON()
    |> writeLines(con = con) )

  close(con)
}

#'
#' Read the entire events file
#'
#' @returns Returns a data frame with columns
#'   * Origin (character)
#'   * Timestamp (POSIXct)
#'   * EventType (character)
#'   * Payload (list-col)
#'   * Alert (logical)
#' @export
#'
loadEvents = function(event_types = NULL) {
  decl(event_types, is.null %or% is.character)

  (loadPostsFile(EVENTS_FILE)
   |> dplyr::filter(is.null(event_types) | (EventType %in% event_types)) )
}

#' @export
latestEvents = function(event_types = NULL) {
  decl(event_types, is.null %or% is.character)

  (loadEvents(event_types = event_types)
   |> dplyr::arrange(Timestamp)
   |> dplyr::group_by(Origin, EventType)
   |> dplyr::slice_tail()
   |> dplyr::ungroup() )
}

#'
#' All events generated today
#'
#' @param ... Passed to [[loadEvents]]
#' @export
#'
todaysEvents = function(...) {
  (loadEvents(...)
   |> dplyr::filter(as.Date(Timestamp) == Sys.Date())
   |> dplyr::arrange(Origin, Timestamp, EventType) )
}

#'
#' All alerts generated today
#'
#' @param ... Passed to [[loadEvents]]
#' @export
#'
todaysAlerts = function(...) {
  (todaysEvents(...)
   |> dplyr::filter(Alert) )
}

# Status handling ----

#'
#' Post a status message to the event log
#'
#' @param origin Name of program posting status (character)
#' @param status Typically "OK" or "Error" or "Failure" (character)
#' @param message Useful, descriptive text message for user (character)
#' @param alert If TRUE, alert user to this status (logical)
#' @seealso [postEvent], which is the underlying writer
#' @export
#'
postStatus = function(origin, status, message, alert = FALSE) {
  postEvent(origin = origin,
          event_type = "status",
          payload = list(status = status,
                         message = message ),
          alert = alert )
}

#' @export
latestStatus = function() {
  (latestEvents(event_types = "status")
   |> dplyr::mutate(Status = purrr::map_chr(Payload, "status"),
                    Message = purrr::map_chr(Payload, "message"))
   |> dplyr::select(Origin, Timestamp, Status, Message, Alert) )
}
