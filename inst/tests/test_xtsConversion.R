context("testing xtsConversion.R functions")

require(xts)

test_that("xtsToRegressionData() basically works", {
    N = 5
    dates = as.Date("2001-01-01") + 1:N
    foo = N:1
    fum = 1:N
    mat = cbind(Foo=foo, Fum=fum)
    x = xts(mat, dates)

    dfrm = xtsToRegressionData(x)

    expect_equal(ncol(dfrm), 3)
    expect_true("date" %in% colnames(dfrm))
    expect_true("symbol" %in% colnames(dfrm))
    expect_true("value" %in% colnames(dfrm))

    expect_equal(nrow(dfrm), 2*N)
})

test_that("xtsToDataFrame() basically works", {
    N = 5
    dates = as.Date("2001-01-01") + 1:N
    foo = N:1
    fum = 1:N
    mat = cbind(Foo=foo, Fum=fum)
    x = xts(mat, dates)

    dfrm = xtsToDataFrame(x)

    expect_equal(ncol(dfrm), ncol(x) + 1)
    expect_true("date" %in% colnames(dfrm))
    expect_true(all(colnames(mat) %in% colnames(dfrm)))

    expect_equal(nrow(dfrm), nrow(x))
})
