context("testing business date functions")

test_that("regular old days are business days", {
    d <- as.Date("2013-02-22")
    expect_true(isBusinessDay(d))
})

test_that("weekends are not business days", {
    sat <- as.Date("2013-02-23")
    sun <- as.Date("2013-02-24")

    expect_false(isBusinessDay(sat))
    expect_false(isBusinessDay(sun))
})

test_that("holidays are not business days", {
    tgiving <- as.Date("2012-11-22")
    expect_false(isBusinessDay(tgiving))

    xmas <- as.Date("2012-12-25")
    expect_false(isBusinessDay(xmas))
})

test_that("business day before weekend is Friday", {
    fri <- as.Date("2013-02-22")
    sat <- as.Date("2013-02-23")
    sun <- as.Date("2013-02-24")

    expect_equivalent(nearestBusinessDay(fri), fri)
    expect_equivalent(nearestBusinessDay(sat), fri)
    expect_equivalent(nearestBusinessDay(sun), fri)
})

test_that("add and subtract business days correctly handles weekends", {
    thu <- as.Date("2013-03-14")
    fri <- as.Date("2013-03-15")
    sat <- as.Date("2013-03-16")
    mon <- as.Date("2013-03-18")
    tue <- as.Date("2013-03-19")

    expect_equivalent(addBusinessDays(thu, 1), fri)
    expect_equivalent(addBusinessDays(thu, 2), mon)

    expect_equivalent(subBusinessDays(fri, 1), thu)
    expect_equivalent(addBusinessDays(fri, 1), mon)
    expect_equivalent(addBusinessDays(fri, 2), tue)

    expect_equivalent(subBusinessDays(sat, 2), thu)
    expect_equivalent(subBusinessDays(sat, 1), fri)
    expect_equivalent(addBusinessDays(sat, 1), mon)
    expect_equivalent(addBusinessDays(sat, 2), tue)

    expect_equivalent(subBusinessDays(mon, 1), fri)
    expect_equivalent(addBusinessDays(mon, 1), tue)
})

test_that("businessDaySeq works over a weekend", {
    thu <- as.Date("2013-04-04")
    fri <- thu + 1
    mon <- fri + 3
    tue <- mon + 1
    wed <- as.Date("2013-04-10")

    seq <- businessDaySeq(thu, 5)
    expect_equal(length(seq), 5)
    expect_true(seq[1] == thu)
    expect_true(seq[2] == fri)
    expect_true(seq[3] == mon)
    expect_true(seq[4] == tue)
    expect_true(seq[5] == wed)
})

test_that("fillBusinessDays fills trailing NAs", {
  thurs = as.Date("2015-02-19")
  fri = as.Date("2015-02-20")
  mon = as.Date("2015-02-23")
  tues = as.Date("2015-02-24")
  NA_DATE = as.Date(NA)

  seq = c(thurs, fri, NA_DATE, NA_DATE)

  filled = fillBusinessDays(seq)

  expect_equal(length(filled), length(seq))
  expect_true(all(!is.na(filled)))

  expect_equal(filled[[3]], mon)
  expect_equal(filled[[4]], tues)
})

test_that("fillBusinessDays fills leading NAs", {
  thurs = as.Date("2015-02-19")
  fri = as.Date("2015-02-20")
  mon = as.Date("2015-02-23")
  tues = as.Date("2015-02-24")
  NA_DATE = as.Date(NA)

  seq = c(NA_DATE, NA_DATE, mon, tues)

  filled = fillBusinessDays(seq)

  expect_equal(length(filled), length(seq))
  expect_true(all(!is.na(filled)))

  expect_equal(filled[[1]], thurs)
  expect_equal(filled[[2]], fri)
})

test_that("businessDaySeq can accept Date parameters", {
  bdays <- businessDaySeq(as.Date("2025-1-1"), as.Date("2025-1-7"))
  expect_equal(bdays, as.Date(c("2025-01-02", "2025-01-03", "2025-01-06", "2025-01-07")))
})

test_that("businessDaySeq can accept character strings", {
  bdays <- businessDaySeq("2025-1-1", "2025-1-7")
  expect_equal(bdays, as.Date(c("2025-01-02", "2025-01-03", "2025-01-06", "2025-01-07")))
})
