context("utilities")

#' \code{"foo$fog$x"} with \code{"x"}


test_that("ConvertCommaSeparatedStringToVector",
    {
        expect_equal(ConvertCommaSeparatedStringToVector("a, a "), c("a","a"))
    })


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







lbls <- c("Q5.  Brand associations: feminine - Coke",
          "Q5.  Brand associations: feminine - Diet Coke",
          "Q5.  Brand associations: feminine - Coke Zero")
lbls2 <- c("Q5.  Brand associations: feminine - Coke",
          "Q5.  Brand associations: feminine - Diet Coke",
          "Q5.  Brand associations: health-conscious - Coke")
test_that("Extract prefix",
    {
        expect_equal(ExtractCommonPrefix(lbls)$common.prefix, "Q5.  Brand associations: feminine")
        expect_equal(ExtractCommonPrefix(lbls)$shortened.labels, c("Coke", "Diet Coke", "Coke Zero"))
        expect_equal(ExtractCommonPrefix(lbls2)$common.prefix, "Q5.  Brand associations")
        expect_equal(ExtractCommonPrefix(lbls2)$shortened.labels, c("feminine - Coke", "feminine - Diet Coke", "health-conscious - Coke"))
    })


test_that("TidyLabels",
    {
        z = "Q5.  Brand associations: feminine"
        expect_equal(TidyLabels(z), z)
        z = c(z, "Q5.  Brand associations: masculine")
        expect_equal(TidyLabels(z), c("Feminine", "Masculine"))
    })

