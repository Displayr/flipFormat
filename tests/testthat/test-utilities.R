context("utilities")

#' \code{"foo$fog$x"} with \code{"x"}

test_that("ReplacingEverythingBefore",
    {
        expect_equal(ReplacingEverythingBefore("foo$fog$x", "\\$"), "x")
        expect_equal(ReplacingEverythingBefore("foo$fog$x", "\\$", FALSE), "$x")
        expect_equal(ReplacingEverythingBefore("GiraffeDogCat", "Dog", FALSE), "DogCat")
        expect_equal(ReplacingEverythingBefore("GiraffeDogCat", "Dog", TRUE), "Cat")
        expect_equal(ReplacingEverythingBefore("a.a", "\\.", TRUE), "a")
        expect_equal(ReplacingEverythingBefore(c("a.a", "bbbbbbbbbbb.bbb"), "\\.", TRUE), c("a", "bbb"))
    })

test_that("Trim white space",
    {
        expect_equal(TrimLeadingWhitepsace("        Big dog         "), "Big dog         ")
        expect_equal(TrimTrailingWhitespace("        Big dog         "), "        Big dog")
        expect_equal(TrimWhitespace("        Big dog         "), "Big dog")
    })

