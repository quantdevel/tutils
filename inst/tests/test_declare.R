context("testing declare function")

test_that("numeric declarations work", {
    takesNumeric <- function(x) { declare(x="numeric") }

    expect_that(takesNumeric(3), equals(NULL))
    expect_that(takesNumeric(c(3,2,1)), equals(NULL))
    expect_that(takesNumeric("foo"), throws_error())
    expect_that(takesNumeric(list(3,2,1)), throws_error())

    # expect_that(x, is_true())
    # expect_that(x, is_false())
    # expect_that(THIS, equals(THAT))
    # expect_that(THIS, is_a(THAT))
    # expect_that(THIS, is_equivalent_to(THAT))
    # expect_that(THIS, is_identical_to(THAT))
    # expect_that(THIS, matches(THAT))
    # expect_that(THIS, prints_text(THAT))
    # expect_that(THIS, shows_message(THAT))
    # expect_that(THIS, gives_warning(THAT))
    # expect_that(THIS, throws_error(THAT))
})

test_that("character declarations work", {
    takesCharacter <- function(x) { declare(x="character") }

    expect_that(takesCharacter("foo"), equals(NULL))
    expect_that(takesCharacter(c("moe","larry","curley")), equals(NULL))
    expect_that(takesCharacter(3), throws_error())
    expect_that(takesCharacter(list(3,2,1)), throws_error())
})

test_that("list declarations work", {
    takesList <- function(x) { declare(x="list") }

    expect_that(takesList(list(3,2,1)), equals(NULL))
    expect_that(takesList(3), throws_error())
    expect_that(takesList("foo"), throws_error())
})

test_that("polytype declarations work", {
    takesEither <- function(x) { declare(x="numeric|character") }

    expect_that(takesEither("foo"), equals(NULL))
    expect_that(takesEither(3), equals(NULL))
    expect_that(takesEither(list(3,2,1)), throws_error())
})
