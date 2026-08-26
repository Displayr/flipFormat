context("CreateCustomTable")

# NOTE: every success-path assertion in this file fetches a real image over the
# network - kImageLink for all but the first block, which also fetches kDropboxLink.
# Without access to the host GET() errors, checkImageTag() warns and strips the tag,
# and those assertions would fail rather than skip, so every block that needs a link
# guards itself with skipIfOffline(). The two blocks that reach no network at all -
# the syntax-error branches, and the decoy whose link is deliberately unresolvable -
# run unguarded. The empty-link assertions also need no network, but they live in the
# first block and skip with it.
#
# The probe fetches the exact links the blocks depend on (once per link per file run,
# cached), rather than using skip_if_offline(): that helper calls skip_on_cran(),
# which would skip these blocks in any run where NOT_CRAN is unset, network or no.
kImageLink <- "https://wiki.q-researchsoftware.com/images/c/cb/CokeZero.png"
kDropboxLink <- "https://www.dropbox.com/s/ukinuuzg0tbojqj/ING%20Logo.png?dl=1"

linkIsReachable <- local(
{
    cached <- list()
    function(link)
    {
        if (is.null(cached[[link]]))
        {
            response <- try(httr::GET(link), silent = TRUE)
            cached[[link]] <<- !inherits(response, "try-error") && response$status_code == 200
        }
        cached[[link]]
    }
})

skipIfOffline <- function(links = kImageLink)
{
    for (link in links)
        skip_if_not(linkIsReachable(link), paste(link, "is not reachable"))
}

kTxtStyle <- paste0("<img style='margin:0 auto;' width='50' src='", kImageLink, "'>")

test_that("checkImageTag",
{
    skipIfOffline(c(kImageLink, kDropboxLink))
    txt.sq <- paste0("<img src='", kDropboxLink, "'>")
    expect_warning(res <- checkImageTag(txt.sq), NA)
    expect_equal(res, paste0("<div>", txt.sq, "</div>"))

    txt.dq <- paste0("<img src=\"", kDropboxLink, "\">")
    expect_warning(res <- checkImageTag(txt.dq), NA)
    expect_equal(res, paste0("<div>", txt.dq, "</div>"))

    txt.empty <- "<img src=''>"
    expect_warning(checkImageTag(txt.empty), "Table content contains an image tag with an empty link")

    txt.empty2 <- "<img src=\"\">"
    expect_warning(checkImageTag(txt.empty2), "Table content contains an image tag with an empty link")

    # the empty-link branch returns sub(imgtag, "", text), not the bare "" the two
    # syntax-error branches return - a difference only visible with surrounding text
    txt.emptyWithText <- "<img src=''>Some text"
    expect_warning(res <- checkImageTag(txt.emptyWithText),
                   "Table content contains an image tag with an empty link")
    expect_equal(res, "Some text")

    txt.badlink <- "<img src='blah'>Some text"
    expect_warning(res <- checkImageTag(txt.badlink),
                   "Table content contains an image tag with an invalid link")
    expect_equal(res, "Some text")

    txt.withattr <- paste0("<a href=https://us.coca-cola.com/products/coca-cola-zero-sugar/ target='_blank'>
         <img src=", kImageLink, " width='45' height='100'></a>")
    expect_warning(res <- checkImageTag(txt.withattr), NA)
    expect_equal(res, paste0("<div>", txt.withattr, "</div>"))
})

test_that("checkImageTag: style and width attributes preceding src are preserved, not stripped",
{
    skipIfOffline()
    expect_warning(res <- checkImageTag(kTxtStyle), NA)
    expect_equal(res, paste0("<div>", kTxtStyle, "</div>"))
})

test_that("checkImageTag: src is found when it is the first attribute",
{
    # not "independent of attribute order" - the decoy block below shows an earlier
    # attribute whose value contains "src=" is matched in preference to the real one
    skipIfOffline()
    txt.order <- paste0("<img src='", kImageLink, "' style='margin:0 auto;' width='50'>")
    expect_warning(res <- checkImageTag(txt.order), NA)
    expect_equal(res, paste0("<div>", txt.order, "</div>"))
})

test_that("checkImageTag: a double-quoted src is cleaned identically to a single-quoted src",
{
    skipIfOffline()
    txt.dq2 <- paste0("<img style='margin:0 auto;' width='50' src=\"", kImageLink, "\">")
    expect_warning(res <- checkImageTag(txt.dq2), NA)
    expect_equal(res, paste0("<div>", txt.dq2, "</div>"))
})

test_that("checkImageTag: an unquoted bare src is cleaned identically",
{
    skipIfOffline()
    txt.unquoted <- paste0("<img style='margin:0 auto;' width='50' src=", kImageLink, ">")
    expect_warning(res <- checkImageTag(txt.unquoted), NA)
    expect_equal(res, paste0("<div>", txt.unquoted, "</div>"))
})

test_that("checkImageTag: both syntax-error branches warn and return an empty string",
{
    # the two branches emit a byte-identical message - each echoes the whole input - so
    # no assertion on the warning can tell them apart. Cover them with one input each:
    # txt.nosrc matches "<img [^>]+>" and so can only reach the src= branch, while
    # txt.notag has no closing ">" and so misses the tag regex first
    txt.nosrc <- "<img style='margin:0 auto;' width='50'>"
    expect_warning(res <- checkImageTag(txt.nosrc),
                   "syntax error which has been removed: <img style='margin:0 auto;' width='50'>")
    expect_equal(res, "")

    txt.notag <- "<img style='margin:0 auto;' width='50'"
    expect_warning(resNoTag <- checkImageTag(txt.notag),
                   "syntax error which has been removed: <img style='margin:0 auto;' width='50'")
    expect_equal(resNoTag, "")
})

test_that("checkImageTag: surrounding text is preserved, not just the image tag, on the success path",
{
    skipIfOffline()
    txt.label <- paste0("Label ", kTxtStyle)
    expect_warning(res <- checkImageTag(txt.label), NA)
    expect_equal(res, paste0("<div>", txt.label, "</div>"))
})

test_that("checkImageTag: an attribute value containing the substring 'src=' before the real src warns of an invalid link rather than silently succeeding",
{
    # regexpr("src=(\\S+)", ...) is unanchored and matches the embedded "src=" inside
    # the alt value before the real src attribute, producing a broken link; this is not
    # asserted as correct behaviour, only that it is not silently treated as valid.
    # Defect RS-23603 - when that is fixed this block should assert the image is kept.
    # The decoy link uses the RFC 2606 reserved .invalid TLD so the GET fails whatever
    # the network does with an unqualified host, and the warning is anchored on the
    # extracted link so a regression in the quote stripping cannot satisfy it. No
    # skipIfOffline() here: the decoy is matched first, so kImageLink is never fetched
    txt.decoy <- paste0("<img alt='mysrc=http://x.invalid/' src='", kImageLink, "'>")
    expect_warning(res <- checkImageTag(txt.decoy),
                   "invalid link which has been removed: http://x\\.invalid/$")
    expect_equal(res, "")
})
