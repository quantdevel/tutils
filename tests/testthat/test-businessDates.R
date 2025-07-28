test_that("businessDaySeq can accept Date parameters", {
  bdays <- businessDaySeq(as.Date("2025-1-1"), as.Date("2025-1-7"))
  expect_equal(bdays, as.Date(c("2025-01-02", "2025-01-03", "2025-01-06", "2025-01-07")))
})

test_that("businessDaySeq can accept character strings", {
  bdays <- businessDaySeq("2025-1-1", "2025-1-7")
  expect_equal(bdays, as.Date(c("2025-01-02", "2025-01-03", "2025-01-06", "2025-01-07")))
})
