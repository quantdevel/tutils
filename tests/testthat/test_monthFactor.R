context("testing monthFactor function")

library(zoo)

test_that("basic stuff works", {
    dates <- c(as.Date(c("2012-03-01", "2012-06-01", "2012-09-01")))
    x <- zoo(1:length(dates), dates)
    expected <- c("Mar", "Jun", "Sep")
    actual <- monthFactor(x)

    expect_equal(expected, as.character(actual))
})
