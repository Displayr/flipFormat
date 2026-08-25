context("CreateCustomTable")

tableHtml <- function(res) res$x$text  # rhtmlMetro::Box stores the emitted HTML in the widget payload's text field
normWs <- function(s) trimws(gsub("\\s+", " ", s))
countOccurrences <- function(pattern, s)
{
    # guard the [[1]]: if the emitted HTML ever arrives as more than one element the
    # counts below would silently only cover the first, and a zero-length s would
    # throw "subscript out of bounds" instead of failing as an expectation
    stopifnot(length(s) == 1L)
    m <- gregexpr(pattern, s, fixed = TRUE)[[1]]
    if (m[1] == -1L) 0L else length(m)
}

xx <- structure(1:5, .Names = c("a", "b", "c", "d", "e"), statistic = "%")
x2 <- matrix(1:12, 4, 3, dimnames = list(letters[1:4], c("X", "Y", "Z")))
test_that("Percentage data",
{
    expect_error(res <- CreateCustomTable(xx), NA)
    expect_equal(attr(res, "ChartData"), xx, check.attributes = FALSE)
    expect_equal(attr(attr(res, "ChartData"), "statistic"), attr(xx, "statistic"))
})

test_that("iframes",
{
    res1 <- CreateCustomTable(xx)
    expect_true(attr(res1, "can-run-in-root-dom"))

    res2 <- CreateCustomTable(xx, custom.css = "table { background-color:green }")
    expect_equal(attr(res2, "can-run-in-root-dom"), NULL)
})

test_that("Runs without column headers",
{
    expect_error(CreateCustomTable(x2, col.widths = '200px',
            col.header.border.width = NULL, border.color = "red"), NA)
})

test_that("Text data is exported correctly",
{
    txt <- c("50%&nbsp;&#8593;", "<b>xxx</b>", "X & Y",
        "He asked me about the votes…Told him I didn’t know anything about it")
    expect_error(res <- CreateCustomTable(txt), NA)
    expect_equal(attr(res, "ChartData"), structure(c("50% ↑", "xxx", "X & Y",
        "He asked me about the votes…Told him I didn’t know anything about it"),
        dim = c(4L, 1L)))
})

# sig.leader.circles -----------------------------------------------------

test_that("No sig.leader.circles emits no circle CSS or divs",
{
    res <- CreateCustomTable(x2)
    expect_false(grepl(".circle", tableHtml(res), fixed = TRUE))
    expect_false(grepl('class="circle', tableHtml(res), fixed = TRUE))
})

test_that("Base circle classes are emitted when sig.leader.circles is supplied",
{
    # circle CSS is value-independent (driven only by non-NULL-ness of sig.leader.circles,
    # circle.size and sig.fills.*); a plain fixture is used so the codes don't misleadingly
    # appear to drive the CSS
    circles <- matrix(0, nrow(x2), ncol(x2))
    res <- CreateCustomTable(x2, sig.leader.circles = circles)
    h <- normWs(tableHtml(res))
    fmt <- "display: inline-block; line-height:35px; border-radius:35px; height: 35px; width:35px;"

    # the trailing " {" is load-bearing: a bare ".circle2" also matches the filled
    # variants ".circle21", ".circle20" and ".circle2-1", so without it the
    # occurrence count would be wrong
    expect_equal(countOccurrences(".circle2 {", h), 1)
    expect_true(grepl(paste0(".circle2 { border: 2px solid rgb(120,120,120);", fmt, "}"), h, fixed = TRUE))

    expect_equal(countOccurrences(".circle1 {", h), 1)
    expect_true(grepl(paste0(".circle1 { border: 1px solid rgb(150,150,150);", fmt, "}"), h, fixed = TRUE))

    expect_equal(countOccurrences(".circle0 {", h), 1)
    expect_true(grepl(paste0(".circle0 { border: 0px solid rgb(0,0,0);", fmt, "}"), h, fixed = TRUE))
})

test_that("All nine filled circle variants are emitted with the correct fill colors",
{
    # circle CSS is value-independent (see "Base circle classes..." above); a plain fixture
    # is used so the codes don't misleadingly appear to drive the CSS
    circles <- matrix(0, nrow(x2), ncol(x2))
    up <- "rgb(1,2,3)"
    nothing <- "rgb(4,5,6)"
    down <- "rgb(7,8,9)"
    res <- CreateCustomTable(x2, sig.leader.circles = circles,
                sig.fills.up = up, sig.fills.nothing = nothing, sig.fills.down = down)
    h <- normWs(tableHtml(res))
    variants <- c("circle21", "circle11", "circle01", "circle20", "circle10",
                  "circle00", "circle2-1", "circle1-1", "circle0-1")

    leader2px <- "2px solid rgb(120,120,120)"
    tie1px <- "1px solid rgb(150,150,150)"
    zero0px <- "0px solid rgb(0,0,0)"
    fmt <- "display: inline-block; line-height:35px; border-radius:35px; height: 35px; width:35px;"
    colors <- rep(c(up, nothing, down), each = 3)
    borders <- rep(c(leader2px, tie1px, zero0px), 3)

    for (i in seq_along(variants))
    {
        v <- variants[i]
        # kept consistent with the base-class assertions; the nine variant names do not
        # collide with each other.
        expect_equal(countOccurrences(paste0(".", v, " {"), h), 1, info = v)
        expected <- paste0(".", v, " { border: ", borders[i], "; background-color:",
                            colors[i], ";", fmt, "}")
        expect_true(grepl(expected, h, fixed = TRUE), info = v)
    }
})

test_that("Every data cell is wrapped in a circle div carrying its own code",
{
    x22 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(2, 1, 0, 2), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- tableHtml(res)
    expect_equal(countOccurrences('<div class="circle', h), prod(dim(x22)))
    expect_true(grepl('<div class="circle2">1</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle1">2</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle0">3</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle2">4</div>', h, fixed = TRUE))
})

test_that("Cell content passes through the circle div wrapping unescaped",
{
    # the contract pinned here is that cell content reaches the HTML verbatim: HTML
    # entities are handed to the browser intact rather than being double-escaped, and
    # a raw "&" is *not* escaped on the way through. If escaping or sanitisation of
    # cell content is ever added this test breaks by design, not as a regression.
    entity <- "50%&nbsp;&#8593;"
    raw.amp <- "X & Y"
    x22 <- matrix(c(entity, raw.amp, "c", "d"), 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(2, 1, 0, 2), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- tableHtml(res)
    expect_true(grepl(paste0('<div class="circle2">', entity, '</div>'), h, fixed = TRUE))
    expect_true(grepl(paste0('<div class="circle1">', raw.amp, '</div>'), h, fixed = TRUE))
})

test_that("sig.leader.circles is recycled against x without warning when the dims disagree",
{
    # index alignment is the whole job of this branch, but nothing validates that
    # sig.leader.circles has the same dim as x (as documented at createcustomtable.R:9).
    # sprintf() silently recycles a shorter codes matrix down the column-major cell
    # order, so the circles land on the wrong cells with no warning. Pinned so that
    # adding validation later is an explicit decision rather than a silent change.
    x22 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(2, 1), 2, 1)
    expect_warning(res <- CreateCustomTable(x22, sig.leader.circles = circles), NA)
    h <- tableHtml(res)
    expect_true(grepl('<div class="circle2">1</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle1">2</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle2">3</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle1">4</div>', h, fixed = TRUE))

    # a length that cannot be recycled dies inside sprintf, with a message that does
    # not mention sig.leader.circles at all
    expect_error(CreateCustomTable(x22, sig.leader.circles = matrix(c(2, 1, 0), 3, 1)),
        "arguments cannot be recycled to the same length")
})

test_that("Out-of-range and NA codes pin the current (buggy) normalisation behaviour",
{
    # See RS-23584. sig.leader.circles[!which(...)] <- 0 negates integer indices
    # rather than inverting a logical mask, so the assignment is a no-op and neither
    # out-of-range codes nor NA are reset to 0 as documented. NA is covered here as well
    # as numerics because a partial fix that clamps out-of-range numbers but leaves NA
    # alone still emits class "circleNA", which matches no CSS rule and so renders no
    # circle at all. This test pins the current behaviour deliberately, pending a fix.
    x22 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(5, 1, -3, NA), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- tableHtml(res)
    expect_true(grepl('<div class="circle5">1</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle1">2</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle-3">3</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circleNA">4</div>', h, fixed = TRUE))
})

test_that("circle.size drives the emitted circle geometry",
{
    # circle CSS is value-independent (see "Base circle classes..." above); a plain fixture
    # is used so the codes don't misleadingly appear to drive the CSS
    circles <- matrix(0, nrow(x2), ncol(x2))
    res <- CreateCustomTable(x2, sig.leader.circles = circles, circle.size = 50)
    h <- normWs(tableHtml(res))
    expect_true(grepl(paste0(".circle2 { border: 2px solid rgb(120,120,120);display: inline-block; ",
                "line-height:50px; border-radius:50px; height: 50px; width:50px;}"), h, fixed = TRUE))
    expect_false(grepl("line-height:35px", h, fixed = TRUE))
    expect_false(grepl("border-radius:35px", h, fixed = TRUE))
})

# spacer.col --------------------------------------------------------------

# the complete predefined .spacer declaration (createcustomtable.R:868), pinned as one
# exact string so that dropping or altering any single declaration fails rather than
# merely checking the selector exists
spacerRule <- ".spacer {background: white;color: white;border: none;overflow:hidden;}"

test_that("Default emits no spacer class",
{
    res <- CreateCustomTable(x2)
    expect_false(grepl('class="spacer"', tableHtml(res), fixed = TRUE))
})

test_that("A single spacer.col index produces exactly one spacer header cell",
{
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    res <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 2)
    h <- tableHtml(res)
    expect_equal(countOccurrences('<th class="spacer">', h), 1)
    # the trailing space before the closing quote is load-bearing: col.header.styles is
    # pasted with the (default empty-string) col.header.classes argument, so the three
    # non-spacer headers carry "colheaderdefault1 " rather than "colheaderdefault1"
    expect_equal(countOccurrences('class="colheaderdefault1 "', h), 3)
})

test_that("The spacer cell replaces, not augments, the default class",
{
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    res <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 2)
    h <- tableHtml(res)
    th <- regmatches(h, regexpr('<th class="[^"]*">X</th>', h))
    expect_equal(th, '<th class="spacer">X</th>')
})

test_that("Multiple spacer.col indices produce spacer cells at those emission positions",
{
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    res <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = c(2, 4))
    h <- tableHtml(res)
    ths <- regmatches(h, gregexpr('<th class="[^"]*">[^<]*</th>', h))[[1]]
    expect_equal(length(ths), 4)
    expect_equal(ths[2], '<th class="spacer">X</th>')
    expect_equal(ths[4], '<th class="spacer">Z</th>')
    expect_equal(ths[1], '<th class="colheaderdefault1 ">W</th>')
    expect_equal(ths[3], '<th class="colheaderdefault1 ">Y</th>')
})

test_that("The spacer cell keeps its column label",
{
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    # spacer.col = 3 (not the sibling blocks' 2) is required here: with columns
    # W, X, Y, Z, index 3 is the one that lands on "Y", which is the label this
    # test needs to demonstrate survives being turned into a spacer cell
    res <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 3)
    h <- tableHtml(res)
    expect_true(grepl('<th class="spacer">Y</th>', h, fixed = TRUE))
})

test_that("Each leading corner cell shifts the spacer.col index by one",
{
    # spacer.col indexes emission position, not data column, and corner cells are prepended
    # to col.header.styles before it is applied. corner.styles[1] is prepended once for
    # show.row.headers (createcustomtable.R:583) and again for row.spans (:589), so the
    # offset is +0, +1 or +2 depending on which of those are set - it is NOT a fixed +1.
    # All three offsets are pinned below against the same spacer.col = 2.
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    ths <- function(res) regmatches(tableHtml(res),
                gregexpr('<th class="[^"]*">[^<]*</th>', tableHtml(res)))[[1]]

    # +0: no corner cells, so index 2 is the second data column
    noCorner <- ths(CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 2))
    expect_equal(length(noCorner), 4)
    expect_equal(noCorner[1], '<th class="colheaderdefault1 ">W</th>')
    expect_equal(noCorner[2], '<th class="spacer">X</th>')

    # +1: the row-header corner takes position 1, so index 2 lands on the FIRST data column
    oneCorner <- ths(CreateCustomTable(m4, show.row.headers = TRUE, spacer.col = 2))
    expect_equal(length(oneCorner), 5)
    expect_equal(oneCorner[1], '<th class="cornerdefault1"></th>')
    expect_equal(oneCorner[2], '<th class="spacer">W</th>')

    # +2: with row.spans a second corner is prepended, so index 2 lands on that corner and
    # every data column is left untouched - the spacer no longer marks a data column at all
    twoCorners <- ths(CreateCustomTable(m4, show.row.headers = TRUE, spacer.col = 2,
                        row.spans = list(list(height = 3, label = "G"))))
    expect_equal(length(twoCorners), 6)
    expect_equal(twoCorners[1], '<th class="cornerdefault1"></th>')
    expect_equal(twoCorners[2], '<th class="spacer"></th>')
    expect_equal(twoCorners[3], '<th class="colheaderdefault1 ">W</th>')
    expect_equal(twoCorners[6], '<th class="colheaderdefault1 ">Z</th>')
})

test_that("col.header.fill reaches the colheaderdefault CSS rule",
{
    res <- CreateCustomTable(x2, col.header.fill = "rgb(1,2,3)")
    h <- normWs(tableHtml(res))
    expect_true(grepl('.colheaderdefault1{ background: rgb(1,2,3);', h, fixed = TRUE))
})

test_that("col.header.fill defaults to transparent",
{
    res <- CreateCustomTable(x2)
    h <- normWs(tableHtml(res))
    expect_true(grepl('.colheaderdefault1{ background: transparent;', h, fixed = TRUE))
})

test_that("show.col.headers = FALSE suppresses the whole header row and its CSS",
{
    res <- CreateCustomTable(x2, show.col.headers = FALSE)
    h <- tableHtml(res)
    # '<th class="' not a bare '<th': <thead> is always emitted, so a bare probe could never fail
    expect_false(grepl('<th class="', h, fixed = TRUE))
    expect_false(grepl('colheaderdefault', h, fixed = TRUE))
})

test_that("spacer.col is silently ignored when show.col.headers is FALSE",
{
    # See RS-23589. The whole spacer.col application lives inside the `if (show.col.headers)`
    # block (createcustomtable.R:592-593), so with the header row off the argument is dropped
    # without warning or error. Pinned so that adding validation - or extending spacer.col
    # to the body - becomes an explicit decision rather than a silent behaviour change.
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    expect_warning(res <- CreateCustomTable(m4, show.col.headers = FALSE, spacer.col = 2), NA)
    h <- tableHtml(res)
    expect_false(grepl('class="spacer"', h, fixed = TRUE))

    # the predefined CSS block is emitted unconditionally, so the .spacer rule is still
    # present with no cell using it. Asserting this separately keeps the negative above
    # honest: it fails because no cell was marked, not because the stylesheet went missing.
    expect_true(grepl(spacerRule, normWs(h), fixed = TRUE))

    # the same call with headers on does mark a cell, confirming the negative flips
    resHeaders <- CreateCustomTable(m4, show.col.headers = TRUE, show.row.headers = FALSE,
                    spacer.col = 2)
    expect_true(grepl('<th class="spacer">X</th>', tableHtml(resHeaders), fixed = TRUE))
})

test_that("Out-of-range spacer.col is pinned to its current (defective) behaviour",
{
    # See RS-23589, which carries both failure modes and the suggested bounds check.
    # col.header.styles[spacer.col] <- "spacer" silently extends the header-style vector
    # with NA for the skipped positions when spacer.col exceeds its length, and the
    # subsequent sprintf() emits a literal class="NA" <th> rather than erroring or being
    # a no-op. spacer.col = 6 is chosen (a multiple of the 3-column header vector) so the
    # sprintf recycling itself does not error - this pins the NA-extension defect itself.
    x2local <- matrix(1:12, 4, 3, dimnames = list(letters[1:4], c("X", "Y", "Z")))
    res <- CreateCustomTable(x2local, show.row.headers = FALSE, spacer.col = 6)
    h <- tableHtml(res)
    expect_equal(countOccurrences('class="NA"', h), 2)
    expect_true(grepl('<th class="spacer">Z</th>', h, fixed = TRUE))
    expect_equal(countOccurrences("<th ", h), 6)

    # spacer.col = 6 survives only because it is a multiple of the 3-column header vector.
    # The off-by-one a caller would actually make - ncols + 1 - is a hard error from
    # sprintf, whose message never mentions spacer.col. Pinned alongside the case above so
    # that both consequences of the missing bounds check are covered, not just the benign one.
    for (k in c(4, 5, 7))
        expect_error(CreateCustomTable(x2local, show.row.headers = FALSE, spacer.col = k),
            "arguments cannot be recycled to the same length", fixed = TRUE, info = k)

    # in-range indices are unaffected, so the errors above are attributable to the
    # out-of-range value rather than to spacer.col generally
    expect_error(CreateCustomTable(x2local, show.row.headers = FALSE, spacer.col = 3), NA)
})

test_that("use.predefined.css = FALSE leaves the spacer header cell with no matching CSS rule",
{
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    res <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 2, use.predefined.css = FALSE)
    h <- tableHtml(res)
    expect_true(grepl('<th class="spacer">', h, fixed = TRUE))
    expect_false(grepl('.spacer {', h, fixed = TRUE))

    # confirm the negative assertion actually flips: with the default use.predefined.css = TRUE
    # the same call does emit the rule - asserted in full, so this cannot be satisfied by a
    # ".spacer {" selector whose declarations have been emptied or changed
    resDefault <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 2)
    expect_true(grepl(spacerRule, normWs(tableHtml(resDefault)), fixed = TRUE))
})

test_that("The predefined spacer rule is emitted in full, once, scoped to the table container",
{
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    res <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 2)
    h <- normWs(tableHtml(res))

    # every declaration pinned, so removing or editing any one of background/color/border/
    # overflow fails here rather than passing on selector presence alone
    expect_true(grepl(spacerRule, h, fixed = TRUE))
    expect_equal(countOccurrences(".spacer {", h), 1)

    # the rule is namespaced under the generated container class rather than defined
    # globally, which is what keeps it from leaking into the rest of the page. The
    # container name is randomised per call, so match its shape rather than its value.
    # Pairing this count with the total above proves the single ".spacer {" occurrence
    # IS the container-scoped one, rather than a global rule sitting alongside it.
    scoped <- regmatches(h, gregexpr("\\.custom-table-container-\\S+ \\.spacer \\{", h))[[1]]
    expect_equal(length(scoped), 1)
})
