#
# API for posting events and alerts
#

POSTS_DIR = "/tau/posts"
STATUS_DIR = file.path(POSTS_DIR, "status")

#' @export
ALERTS_PATH = file.path(POSTS_DIR, "alerts.jsonl")

# OBSOLETE
#' @export
EVENTS_FILE = file.path(POSTS_DIR, "events.jsonl")

# Local functions ----


# Status handling ----

#'
#' Post a status message
#'
#' Post the status of an origin, including a helpful message.
#'
#' Note that posting a new status will overwrite any existing status.
#'
#' @param origin Name of program that is posting status (character)
#' @param status Typically "OK" or "Error" or "Failure" (character)
#' @param message Useful, descriptive text message for user
#'    (optional, character)
#' @param url URL of related app page, if any
#'    (optional, character)
#' @param link_text Link text for anchor of related app page, if any
#'    (optional, character)
#' @returns Nothing
#' @seealso See [readStatus] for reading status messages.
#' @export
#'
postStatus = function(origin, status,
                      message = NULL,
                      url = NA_character_,
                      link_text = NA_character_ ) {
  decl(origin, is.character)
  decl(status, is.character)
  decl(message, is.null %or% is.character)
  decl(url, is.character)
  decl(link_text, is.character)

  timestamp <- Sys.time()
  message <- message %||% paste0(origin, ": ", status)

  con <- file(status_file_path(origin), open = "w")

  (list(Origin = origin,
        Timestamp = format(timestamp),
        Status = status,
        Message = message,
        URL = url,
        LinkText = link_text )
    |> jsonlite::toJSON()
    |> writeLines(con = con) )

  close(con)
  invisible(NULL)
}

#'
#' @title Read Tau status postings
#' @description
#' blah blah blah
#' @param origin Limit to postings from this origin (optional, character)
#' @returns Returns a data frame with columns
#'    - Origin (character)
#'    - Timestamp (POSIXct)
#'    - Status (character)
#'    - Message (character or NA)
#'    - URL (character or NA)
#'    - LinkText (character or NA)
#'
#' Returns an empty data frame if no status postings are available.
#' @seealso [postStatus] for posting a status in the first place
#' @export
#'
readStatus = function(origin = NULL) {
  decl(origin, is.null %or% is.character)

  fpaths <-
    if (is.null(origin)) {
      (STATUS_DIR
       |> list.files(pattern = ".json", full.names = TRUE) )
    } else {
      (status_file_path(origin)
       |> purrr::keep(file.exists) )
    }

  if (length(fpaths) > 0) {
    (fpaths
     |> purrr::map(read_status_file)
     |> purrr::list_rbind() )
  } else {
    empty_status()
  }
}

status_file_path = function(origin) {
  file.path(STATUS_DIR, paste0(origin, ".json"))
}

read_status_file = function(fpath) {
  con <- file(fpath, open = "r")

  make_tibble = function(lst) {
    tibble::tibble(
      Origin = lst$Origin,
      Timestamp = as.POSIXct(lst$Timestamp),
      Status = lst$Status,
      Message = lst$Message,
      URL = lst$URL,
      LinkText = lst$LinkText )
  }

  rows <- (readLines(con)
           |> purrr::map(jsonlite::fromJSON)
           |> purrr::map(make_tibble)
           |> purrr::list_rbind() )

  close(con)
  return(rows)
}

empty_status = function() {
  tibble::tibble(Origin = character(0),
                 Timestamp = as.POSIXct(character(0)),
                 Status = character(0),
                 Message = character(0),
                 URL = character(0),
                 LinkText = character(0) )
}

# Alert handling ----

#'
#' @title Post an alert
#' @description
#' (TBD)
#' @param origin (character)
#' @param topic (optional, character)
#' @param message (optional, character)
#' @param expiration Date or time at which alert expires,
#'   default to next business day after the current trading day
#'   (optional, Date or POSIXt)
#' @param url (optional, character)
#' @param link_text (optional, character)
#' @returns Nothing
#' @seealso [readAlerts] to read all open alerts
#' @export
#'
postAlert = function(origin, message,
                     expiration = NULL,
                     topic = NA_character_,
                     url = NA_character_,
                     link_text = NA_character_ ) {
  decl(origin, is.character)
  decl(message, is.character)
  decl(expiration, is.null %or% lubridate::is.Date %or% lubridate::is.POSIXt)
  decl(topic, is.character)
  decl(url, is.character)
  decl(link_text, is.character)

  timestamp <- Sys.time()
  expiration <- expiration %||% tutils::nextBusinessDay(tutils::thisTradingDay())

  con <- file(ALERTS_PATH, open = "a")

  (list(Origin = origin,
        Timestamp = format(timestamp),
        Message = message,
        Expiration = format(expiration),
        Topic = topic,
        URL = url,
        LinkText = link_text )
    |> jsonlite::toJSON()
    |> writeLines(con = con) )

  close(con)
  invisible(NULL)
}

#'
#' @title Read all non-expired alerts
#' @description
#' Return all open alerts; that is, alerts which have not yet expired.
#'
#' For a given Origin and Topic, only the most recent alert is returned.
#'
#' @returns Returns a data frame with columns
#'   - Origin (character)
#'   - Timestamp (POSIXct)
#'   - Message (character)
#'   - Expiration (POSIXct)
#'   - Topic (character or NA)
#'   - URL (character or NA)
#'   - LinkText (character or)
#'
#' The data frame will be empty if there are no active alerts.
#'
#' @seealso [postAlert] for posting an alert in the first place.
#' @export
#'
readAlerts = function() {
  if (!file.exists(ALERTS_PATH)) {
    return(empty_alerts())
  }

  con <- file(ALERTS_PATH, open = "r")

  make_tibble = function(lst) {
    tibble::tibble(
      Origin = lst$Origin,
      Timestamp = as.POSIXct(lst$Timestamp),
      Message = lst$Message,
      Expiration = as.POSIXct(lst$Expiration),
      Topic = lst$Topic,
      URL = lst$URL,
      LinkText = lst$LinkText )
  }

  lines <- (readLines(con)
            |> purrr::map(jsonlite::fromJSON)
            |> purrr::map(make_tibble)
            |> purrr::list_rbind()
            |> dplyr::filter(Expiration > Sys.time())
            |> dplyr::slice_tail(n = 1, by = c(Origin, Topic)) )

  close(con)
  return(lines)
}

empty_alerts = function() {
  tibble::tibble(Origin = character(0),
                 Timestamp = as.POSIXct(character(0)),
                 Message = character(0),
                 Topic = character(0),
                 Expiration = as.POSIXct(character(0)),
                 URL = character(0),
                 LinkText = character(0) )
}
