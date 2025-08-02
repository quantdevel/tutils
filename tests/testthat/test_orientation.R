context("testing landscape() and portrait() functions")

test_that("landscape() orients matrices correctly", {
    wide = matrix(1, nrow=2, ncol=5)
    orient = landscape(wide)

    expect_that(orient, is_a("matrix"))
    expect_equal(dim(orient), c(2, 5))

    narrow = matrix(1, nrow=5, ncol=2)
    orient = landscape(narrow)

    expect_that(orient, is_a("matrix"))
    expect_equal(dim(orient), c(2, 5))
})

test_that("landscape() orients data frames correctly", {
    wide = data.frame(x1=1:2, x2=1:2, x3=1:2, x4=1:2, x5=1:2)
    orient = landscape(wide)

    expect_that(orient, is_a("data.frame"))
    expect_equal(dim(orient), c(2, 5))

    narrow = data.frame(x1=1:5, x2=1:5)
    orient = landscape(narrow)

    expect_that(orient, is_a("data.frame"))
    expect_equal(dim(orient), c(2, 5))
})

test_that("portrait() orients matrices correctly", {
    wide = matrix(1, nrow=2, ncol=5)
    orient = portrait(wide)

    expect_that(orient, is_a("matrix"))
    expect_equal(dim(orient), c(5, 2))

    narrow = matrix(1, nrow=5, ncol=2)
    orient = portrait(narrow)

    expect_that(orient, is_a("matrix"))
    expect_equal(dim(orient), c(5, 2))
})

test_that("portrait() orients data frames correctly", {
    wide = data.frame(x1=1:2, x2=1:2, x3=1:2, x4=1:2, x5=1:2)
    orient = portrait(wide)

    expect_that(orient, is_a("data.frame"))
    expect_equal(dim(orient), c(5, 2))

    narrow = data.frame(x1=1:5, x2=1:5)
    orient = portrait(narrow)

    expect_that(orient, is_a("data.frame"))
    expect_equal(dim(orient), c(5, 2))
})
