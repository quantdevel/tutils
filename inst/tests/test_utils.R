context("testing utility functions")

test_that("coalesce works as expected", {
    expect_that(coalesce(), equals(NULL))
    expect_that(coalesce(NULL), equals(NULL))
    expect_that(coalesce(NULL, NULL), equals(NULL))
    expect_that(coalesce(NULL, "foo"), equals("foo"))
    expect_that(coalesce("foo", NULL), equals("foo"))
    expect_that(coalesce("foo", "fum"), equals("foo"))
})

test_that("the 'the' function works as expected", {
    expect_that(the(c(5)), equals(5))
    expect_that(the(list(5)), equals(5))

    expect_that(the(c()), throws_error())
    expect_that(the(c(5,4,3)), throws_error())
    expect_that(the(list()), throws_error())
    expect_that(the(list(5,4,3)), throws_error())
})
