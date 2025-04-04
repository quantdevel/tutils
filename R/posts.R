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
      Alert = lst$Alert,
      Message = lst$Message,
      URL = lst$URL,
      LinkText = lst$LinkText,
      Payload = list(lst$Payload) )
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
#' @param status (character)
#' @param alert If TRUE, this event will be brought to the attention
#'   of the system user
#' @param message (character)
#' @param url (character)
#' @param link_text (character)
#' @param payload Any R object which can be serialized into JSON format
#' @seealso To post a *status* event, call [postStatus].
#'
#' To read events, try [latestEvents] and [todaysEvents].
#' To read status messages, try [latestStatus].
#' To read alert messages, try [latestAlerts] and [todaysAlerts].
#' @export
#'
postEvent = function(origin, event_type, status,
                     alert = FALSE,
                     message = NA_character_,
                     url = NA_character_,
                     link_text = NA_character_,
                     payload = list()) {
  decl(origin, is.character)
  decl(event_type, is.character)
  decl(status, is.character)
  decl(alert, is.logical)
  decl(message, is.character)
  decl(url, is.character)
  decl(link_text, is.character)

  timestamp <- Sys.time()
  con <- file(EVENTS_FILE, open = "a")

  (list(Origin = origin,
        Timestamp = timestamp,
        EventType = event_type,
        Alert = alert,
        Message = message,
        URL = url,
        LinkText = link_text,
        Payload = payload )
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
#' @seealso This is a lower-level function.
#'   You probably want to call [latestEvents] or [todaysEvents] instead.
#' @export
#'
loadEvents = function(event_types = NULL) {
  decl(event_types, is.null %or% is.character)

  (loadPostsFile(EVENTS_FILE)
   |> dplyr::filter(is.null(event_types) | (EventType %in% event_types)) )
}

#'
#' Read the latest event messages.
#'
#' @param event_types Limit to these event types;
#'   NULL means all events (NULL or character)
#' @seealso [loadEvents] describes the return type.
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
#' @returns See [loadEvents], the underlying function
#' @seealso [todaysEvents] for events that happened today
#' @export
#'
todaysEvents = function(...) {
  (loadEvents(...)
   |> dplyr::filter(as.Date(Timestamp) == Sys.Date())
   |> dplyr::arrange(Origin, Timestamp, EventType) )
}


# Status handling ----

#'
#' Post a status message to the event log
#'
#' @param origin Name of program posting status (character)
#' @param status Typically "OK" or "Error" or "Failure" (character)
#' @param message Useful, descriptive text message for user
#'    (optional, character)
#' @param alert If TRUE, alert user to this status (logical)
#' @param message (optional, character)
#' @param url (optional, character)
#' @param link_text (optional, character)
#' @seealso See [latestStatus] for reading status messages.
#' See [postEvent] for writing non-status messages.
#' @export
#'
postStatus = function(origin, status, alert = FALSE,
                      message = NA_character_,
                      url = NA_character_,
                      link_text = NA_character_ ) {
  decl(origin, is.character)
  decl(status, is.character)
  decl(alert, is.logical)
  decl(message, is.null %or% is.character)
  decl(url, is.character)
  decl(link_text, is.character)

  message <- message %||% NA_character_

  postEvent(origin = origin,
          event_type = "status",
          status = status,
          alert = alert,
          message = message,
          url = url,
          link_text = link_text )
}

#'
#'  Most-recent status events
#'
#' @returns Returns a data frame with columns
#'    * Origin
#'    * Timestamp
#'    * Status
#'    * Message
#'    * Alert
#' @seealso [postStatus] for posting status messages.
#' @export
#'
latestStatus = function() {
  (latestEvents(event_types = "status")
   |> dplyr::mutate(Status = purrr::map_chr(Payload, "status"),
                    Message = purrr::map_chr(Payload, "message"))
   |> dplyr::select(Origin, Timestamp, Status, Message, Alert) )
}

# Alert handling ----

#'
#' Most-recent events that are alerts
#'
#' Returns most-recent events that have `Alert` set to `TRUE`.
#'
#' @param event_types If specified, limit to events of these types
#'   (optional, character vector)
#' @seealso See [latestEvents] for reading all events.
#' @export
#'
latestAlerts = function(event_types = NULL) {
  decl(event_types, is.null %or% is.character)

  (latestEvents(event_types = event_types)
    |> dplyr::filter(Alert)
    |> dplyr::mutate(Alert = NULL) )
}

#'
#' All alerts generated today
#'
#' @param ... Passed to [[loadEvents]]
#' @seealso [postAlert] and [latestAlerts]
#' @export
#'
todaysAlerts = function(...) {
  (todaysEvents(...)
   |> dplyr::filter(Alert)
   |> dplyr::mutate(Alert = NULL) )
}
