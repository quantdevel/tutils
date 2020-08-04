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
  ensure(dates, is.Date)

  as.logical(RQuantLib::isBusinessDay(DEFAULT_CALENDAR, dates=dates))
}

#'
#'  Generate vector of business dates
#'
#' @param from Date or numeric;
#'   numeric is number of days before \code{to} date
#' @param to Date
#' @return Vector of Date objects
#' @export
#'
businessCalendar = function(from=START_OF_TIME, to=Sys.Date()) {
  ensure(from, is.numeric(.) || is.Date(.))
  ensure(to, is.Date)

  if (is.Date(from)) {
    start <- from
  } else {
    start <- subBusinessDays(to, from)
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
  declare(dates="Date")
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
  declare(dates="Date", n="integer|numeric", timeUnit="integer|numeric")
  RQuantLib::advance(calendar=DEFAULT_CALENDAR, dates=dates, n=n, timeUnit=timeUnit)
}

#' @export addBusinessDays subBusinessDays
#' @export addBusinessWeeks subBusinessWeeks
#' @export addBusinessMonths subBusinessMonths
#' @export addBusinessYears subBusinessYears

addBusinessDays = function(dates, n) addBusinessTimeUnits(dates, n, 0)
subBusinessDays = function(dates, n) addBusinessTimeUnits(dates, -(n), 0)
addBusinessWeeks = function(dates, n) addBusinessTimeUnits(dates, n, 1)
subBusinessWeeks = function(dates, n) addBusinessTimeUnits(dates, -(n), 1)
addBusinessMonths = function(dates, n) addBusinessTimeUnits(dates, n, 2)
subBusinessMonths = function(dates, n) addBusinessTimeUnits(dates, -(n), 2)
addBusinessYears = function(dates, n) addBusinessTimeUnits(dates, n, 3)
subBusinessYears = function(dates, n) addBusinessTimeUnits(dates, -(n), 3)

#' @export prevBusinessDay nextBusinessDay
prevBusinessDay = function(dates) addBusinessTimeUnits(dates, -1, 0)
nextBusinessDay = function(dates) addBusinessTimeUnits(dates, +1, 0)

#'
#'  Create sequence of business dates
#'
#' @param from A Date object
#' @param horizon Either a number of dates or an end Date
#' @return Vector of Date objects
#'
#' @export
#'
businessDaySeq = function(from, horizon) {
  declare(from="Date", horizon="integer|numeric|Date")

  if (is.numeric(horizon)) {
    stopifnot(horizon > 0)

    if (horizon == 1) return(from)
    seq <- rep(from, horizon)     # Just creating a vector of N dates
    for (i in 2:horizon) {
      seq[i] <- nextBusinessDay(seq[i-1])
    }
  } else {
    # This can happen, for example, if expiration dates get screwed up
    if (horizon < from) stop("businessDaySeq: 'horizon' is earlier than 'from'")

    seq <- list()
    while (from <= horizon) {
      seq[length(seq) + 1] <- from
      from <- nextBusinessDay(from)
    }
    seq <- unlist(seq)
  }
  as.Date(seq)
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
  declare(dates="Date")

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
