context("CreateCustomTable")

# NOTE: the success-path blocks below (style/width attribute preservation, attribute
# order, quoting variants, surrounding text) fetch a real image from
# wiki.q-researchsoftware.com. Without network access to that host GET() errors,
# checkImageTag() warns and strips the tag, and those assertions would fail rather
# than skip - so each such block guards itself with skipIfOffline().
#
# The probe fetches the exact image the blocks depend on (once per file run, cached),
# rather than using skip_if_offline(): that helper calls skip_on_cran(), which would
# skip these blocks in any run where NOT_CRAN is unset, network or no.
kImageLink <- "https://wiki.q-researchsoftware.com/images/c/cb/CokeZero.png"

imageIsReachable <- local(
{
    cached <- NULL
    function()
    {
        if (is.null(cached))
        {
            response <- try(httr::GET(kImageLink), silent = TRUE)
            cached <<- !inherits(response, "try-error") && response$status_code == 200
        }
        cached
    }
})

skipIfOffline <- function()
    skip_if_not(imageIsReachable(), paste(kImageLink, "is not reachable"))

kTxtStyle <- paste0("<img style='margin:0 auto;' width='50' src='", kImageLink, "'>")

test_that("checkImageTag",
{
    txt.sq <- "<img src='https://www.dropbox.com/s/ukinuuzg0tbojqj/ING%20Logo.png?dl=1'>"
    expect_error(res <- checkImageTag(txt.sq), NA)
    expect_equal(nchar(res), 84)

    txt.dq <- "<img src=\"https://www.dropbox.com/s/ukinuuzg0tbojqj/ING%20Logo.png?dl=1\">"
    expect_error(res <- checkImageTag(txt.dq), NA)
    expect_equal(nchar(res), 84)

    txt.empty <- "<img src=''>"
    expect_warning(checkImageTag(txt.empty), "Table content contains an image tag with an empty link")

    txt.empty2 <- "<img src=\"\">"
    expect_warning(checkImageTag(txt.empty2), "Table content contains an image tag with an empty link")


    txt.badlink <- "<img src='blah'>Some text"
    expect_warning(res <- checkImageTag(txt.badlink),
                   "Table content contains an image tag with an invalid link")
    expect_equal(res, "Some text")

    txt.withattr <- "<a href=https://us.coca-cola.com/products/coca-cola-zero-sugar/ target='_blank'>
         <img src=https://wiki.q-researchsoftware.com/images/c/cb/CokeZero.png width='45' height='100'></a>"
    expect_error(res <- checkImageTag(txt.withattr), NA)
    expect_equal(res, paste0("<div>", txt.withattr, "</div>"))
})

test_that("checkImageTag: style and width attributes preceding src are preserved, not stripped",
{
    skipIfOffline()
    expect_warning(res <- checkImageTag(kTxtStyle), NA)
    expect_equal(res, paste0("<div>", kTxtStyle, "</div>"))
})

test_that("checkImageTag: src location is independent of attribute order",
{
    skipIfOffline()
    txt.order <- "<img src='https://wiki.q-researchsoftware.com/images/c/cb/CokeZero.png' style='margin:0 auto;' width='50'>"
    expect_error(res <- checkImageTag(txt.order), NA)
    expect_equal(res, paste0("<div>", txt.order, "</div>"))
})

test_that("checkImageTag: a double-quoted src is cleaned identically to a single-quoted src",
{
    skipIfOffline()
    txt.dq2 <- "<img style='margin:0 auto;' width='50' src=\"https://wiki.q-researchsoftware.com/images/c/cb/CokeZero.png\">"
    expect_error(res <- checkImageTag(txt.dq2), NA)
    expect_equal(res, paste0("<div>", txt.dq2, "</div>"))
})

test_that("checkImageTag: an unquoted bare src is cleaned identically",
{
    skipIfOffline()
    txt.unquoted <- "<img style='margin:0 auto;' width='50' src=https://wiki.q-researchsoftware.com/images/c/cb/CokeZero.png>"
    expect_error(res <- checkImageTag(txt.unquoted), NA)
    expect_equal(res, paste0("<div>", txt.unquoted, "</div>"))
})

test_that("checkImageTag: attributes with no src attribute at all warns with a syntax error and returns an empty string",
{
    txt.nosrc <- "<img style='margin:0 auto;' width='50'>"
    expect_warning(res <- checkImageTag(txt.nosrc),
                   "syntax error which has been removed: <img style='margin:0 auto;' width='50'>")
    expect_equal(res, "")
})

test_that("checkImageTag: surrounding text is preserved, not just the image tag, on the success path",
{
    skipIfOffline()
    txt.label <- paste0("Label ", kTxtStyle)
    expect_error(res <- checkImageTag(txt.label), NA)
    expect_equal(res, paste0("<div>", txt.label, "</div>"))
})

test_that("checkImageTag: an attribute value containing the substring 'src=' before the real src warns of an invalid link rather than silently succeeding",
{
    # regexpr("src=(\\S+)", ...) is unanchored and matches the embedded "src=" inside
    # "mysrc=x" before the real src attribute, producing a broken link; this is not
    # asserted as correct behaviour, only that it is not silently treated as valid.
    # Defect RS-23603 - when that is fixed this block should assert the image is kept.
    # The warning is anchored on the extracted link so a regression in the quote
    # stripping (which would give "x'") cannot satisfy it.
    txt.decoy <- "<img alt='mysrc=x' src='https://wiki.q-researchsoftware.com/images/c/cb/CokeZero.png'>"
    expect_warning(res <- checkImageTag(txt.decoy), "invalid link which has been removed: x$")
    expect_equal(res, "")
})
