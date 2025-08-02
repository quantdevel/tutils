context("testing utility functions")

test_that("the 'the' function works as expected", {
    expect_that(the(c(5)), equals(5))
    expect_that(the(list(5)), equals(5))

    expect_that(the(c()), throws_error())
    expect_that(the(c(5,4,3)), throws_error())
    expect_that(the(list()), throws_error())
    expect_that(the(list(5,4,3)), throws_error())
})
