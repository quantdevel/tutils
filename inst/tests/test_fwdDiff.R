context("testing fwdDiff function")

library(xts)

test_that("basic forward diff works", {
    x = xts::xts(5:1, Sys.Date() + 1:5)

    fd = fwdDiff(x, 2)

    expect_is(fd, "xts")
    expect_equal(index(fd), index(fd))
    expect_equal(coredata(fd), rbind(-2, -2, -2, NA, NA))
})

#
# For your reference:
#
# expect_that(x, is_true())            expect_true(x)
# expect_that(x, is_false())           expect_false(x)
# expect_that(x, is_a(y))              expect_is(x, y)
# expect_that(x, equals(y))            expect_equal(x, y)
# expect_that(x, is_equivalent_to(y))  expect_equivalent(x, y)
# expect_that(x, is_identical_to(y))   expect_identical(x, y)
# expect_that(x, matches(y))           expect_matches(x, y)
# expect_that(x, prints_text(y))       expect_output(x, y)
# expect_that(x, shows_message(y))     expect_message(x, y)
# expect_that(x, gives_warning(y))     expect_warning(x, y)
# expect_that(x, throws_error(y))      expect_error(x, y)
#
