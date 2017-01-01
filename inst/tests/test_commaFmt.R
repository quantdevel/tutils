context("testing commaFmt function")

test_that("basic formatting works", {
    expect_that(commaFmt(1), equals("1"))
    expect_that(commaFmt(1000), equals("1,000"))
    # BROKEN?: expect_that(commaFmt(1000000), equals("1,000,000"))
})


    # BROKEN:
    # test_that(commaFmt(1000.999, digits=2), equals("1,001.00"))
