DEFAULT_CALENDAR = "UnitedStates"
DEFAULT_TIMEZONE = "America/Chicago"
END_OF_TRADING_DAY = 12 + 7   # hour of day; CSI claims futures posted by 7:10 PM Central

#' @export START_OF_TIME
START_OF_TIME = as.Date("1987-01-01")

#'
#'  Predicate for business days
#'
#' @param dates Vector of Date objects
#' @return Logical vector
#' @export
#'
isBusinessDay = function(dates) {
  decl(dates, lubridate::is.Date)

  as.logical(RQuantLib::isBusinessDay(DEFAULT_CALENDAR, dates=dates))
}

#'
#'  Generate vector of business dates
#'
#' @param from Date, or character string in standard date format;
#'   or numeric, meaning the number of days before `to` date
#' @param to Date or character string in standard date format
#' @return Vector of Date objects
#' @export
#'
businessCalendar = function(from = START_OF_TIME, to = NULL) {
  decl(from, is.numeric %or% lubridate::is.Date %or% is.character)
  to <- as.Date(to %||% Sys.Date())

  if (is.numeric(from)) {
    start <- subBusinessDays(to, from)
  } else {
    start <- as.Date(from)
  }

  allDates <- seq(start, by=1, to=to)
  allDates[isBusinessDay(allDates)]
}

#'
#'  Find business date nearest to given date
#'
#' @param dates  Vector of Date objects
#' @return Vector of Date objects
#' @export
#'
nearestBusinessDay = function(dates = Sys.Date()) {
  decl(dates, lubridate::is.Date)
  RQuantLib::adjust(calendar=DEFAULT_CALENDAR, dates=dates, bdc=2)
}

#'
#'  Previous trading day
#'
#' @return Date object
#' @export
#'
prevTradingDay = function() {
  now <- Sys.time()
  today <- as.Date(now, tz=DEFAULT_TIMEZONE)

  hour = as.POSIXlt(now, tz=DEFAULT_TIMEZONE)$hour
  if (hour < END_OF_TRADING_DAY) {
    nearestBusinessDay(today - 1)
  } else {
    nearestBusinessDay(today)
  }
}

#'
#'  Current trading day
#'
#' @return Date object
#' @export
#'
thisTradingDay = function() {
  now <- Sys.time()
  today <- as.Date(now, tz=DEFAULT_TIMEZONE)
  busday <- nearestBusinessDay(today)

  hour = as.POSIXlt(now, tz=DEFAULT_TIMEZONE)$hour
  if (today > busday || hour >= END_OF_TRADING_DAY) {
    busday <- nextBusinessDay(busday)
  }
  return(busday)
}

#'
#' Add business time units
#'
#' @param dates Vector of Date objects
#' @param n Number of units to add
#' @param timeUnit Units of time: 0 = days, 1 = weeks, 2 = months, 3 = years
#' @export
#'
addBusinessTimeUnits = function(dates, n, timeUnit) {
  decl(dates, lubridate::is.Date)
  decl(n, is.numeric)
  decl(timeUnit, is.numeric)

  RQuantLib::advance(calendar=DEFAULT_CALENDAR, dates=dates, n=n, timeUnit=timeUnit)
}

#'
#'  Business unit date arithmetic
#'
#'  Given a vector of dates, add or subtract
#'  business date units from it: days, weeks, months, or years.
#'
#' @param dates Vector of Date objects
#' @param n Number ov days, weeks, months, or years
#'   to either add or subtract (integer)
#' @name bus-date-arith
#'
NULL

#' @rdname bus-date-arith
#' @export
addBusinessDays = function(dates, n) addBusinessTimeUnits(dates, n, 0)

#' @rdname bus-date-arith
#' @export
subBusinessDays = function(dates, n) addBusinessTimeUnits(dates, -(n), 0)

#' @rdname bus-date-arith
#' @export
addBusinessWeeks = function(dates, n) addBusinessTimeUnits(dates, n, 1)

#' @rdname bus-date-arith
#' @export
subBusinessWeeks = function(dates, n) addBusinessTimeUnits(dates, -(n), 1)

#' @rdname bus-date-arith
#' @export
addBusinessMonths = function(dates, n) addBusinessTimeUnits(dates, n, 2)

#' @rdname bus-date-arith
#' @export
subBusinessMonths = function(dates, n) addBusinessTimeUnits(dates, -(n), 2)

#' @rdname bus-date-arith
#' @export
addBusinessYears = function(dates, n) addBusinessTimeUnits(dates, n, 3)

#' @rdname bus-date-arith
#' @export
subBusinessYears = function(dates, n) addBusinessTimeUnits(dates, -(n), 3)

#'
#'  Previous or next business day
#'
#' @param dates Vector of Date objects
#' @returns `prevBusinessDay` returns the vector of
#'   corresponding previous business days.
#'
#'   `nextBusinessDay` returns the vector of
#'   corresponding next business days.
#' @export
#'
prevBusinessDay = function(dates) addBusinessTimeUnits(dates, -1, 0)

#' @rdname prevBusinessDay
#' @export
nextBusinessDay = function(dates) addBusinessTimeUnits(dates, +1, 0)

#'
#'  Create sequence of business dates
#'
#' @param from A Date object or character string in standard date format
#' @param horizon Either a number of dates or an end Date
#' @return Vector of Date objects
#'
#' @export
#'
businessDaySeq = function(from, horizon) {
  from <- as.Date(from)
  decl(horizon, is.numeric %or% lubridate::is.Date %or% is.character)

  if (is.numeric(horizon)) {
    stopifnot(horizon > 0)

    if (horizon == 1) {
      from
    } else {
      purrr::accumulate(.x = 2:horizon,
                        .f = \(prev, i) nextBusinessDay(prev),
                        .init = from )
    }

    # seq <- rep(from, horizon)     # Just creating a vector of N dates
    # for (i in 2:horizon) {
    #   seq[i] <- nextBusinessDay(seq[i-1])
    # }
  } else {
    businessCalendar(from, as.Date(horizon))

    # # This can happen, for example, if expiration dates get screwed up
    # if (horizon < from) stop("businessDaySeq: 'horizon' is earlier than 'from'")
    #
    # seq <- list()
    # while (from <= horizon) {
    #   seq[length(seq) + 1] <- from
    #   from <- nextBusinessDay(from)
    # }
    # seq <- unlist(seq)
    # as.Date(seq)
  }
}

#'
#'  Replace missing business days
#'
#'  Given a vector of Dates with NAs at the end,
#'     replace the NAs with business days
#'
#' @param dates A vector of Dates
#' @return Same vector, with trailing NAs replaced by business days
#'
#' @export
#'
fillBusinessDays = function(dates) {
  decl(dates, lubridate::is.Date)

  stopifnot(any(!is.na(dates)))  # Must have at least one known date

  N = length(dates)

  knowns = !is.na(dates)
  firstKnown = min(which(knowns))
  if (firstKnown > 1) {
    nMissing = firstKnown - 1
    padStart = subBusinessDays(dates[firstKnown], nMissing)
    padding = businessDaySeq(padStart, nMissing)
    dates = c(padding, dates[firstKnown:N])
  }

  lastKnown = max(which(knowns))
  if (lastKnown < N) {
    firstMissing = lastKnown + 1
    padStart = nextBusinessDay(dates[lastKnown])
    padding = businessDaySeq(padStart, N - lastKnown)
    dates = c(dates[1:lastKnown], padding)
  }

  return(dates)
}
