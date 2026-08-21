context("CreateCustomTable")

tableHtml <- function(res) res$x$text  # rhtmlMetro::Box stores the emitted HTML in the widget payload's text field
normWs <- function(s) trimws(gsub("\\s+", " ", s))
countOccurrences <- function(pattern, s)
{
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
    circles <- matrix(0, 4, 3)
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
    circles <- matrix(0, 4, 3)
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

test_that("Rendered cell text is preserved inside the circle div wrapping",
{
    txt <- "X & Y"
    x22 <- matrix(c(txt, "b", "c", "d"), 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(2, 1, 0, 2), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- tableHtml(res)
    expect_true(grepl(paste0('<div class="circle2">', txt, '</div>'), h, fixed = TRUE))
})

test_that("Out-of-range codes pin the current (buggy) normalisation behaviour",
{
    # sig.leader.circles[!which(...)] <- 0 negates integer indices rather than
    # inverting a logical mask, so out-of-range codes are not reset to 0 as documented.
    # No defect ticket has been filed for this yet; this test pins that behaviour
    # deliberately, pending a fix.
    x22 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(5, 1, -3, 2), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- tableHtml(res)
    expect_true(grepl('<div class="circle5">1</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle-3">3</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle1">2</div>', h, fixed = TRUE))
})

test_that("circle.size drives the emitted circle geometry",
{
    # circle CSS is value-independent (see "Base circle classes..." above); a plain fixture
    # is used so the codes don't misleadingly appear to drive the CSS
    circles <- matrix(0, 4, 3)
    res <- CreateCustomTable(x2, sig.leader.circles = circles, circle.size = 50)
    h <- normWs(tableHtml(res))
    expect_true(grepl(paste0(".circle2 { border: 2px solid rgb(120,120,120);display: inline-block; ",
                "line-height:50px; border-radius:50px; height: 50px; width:50px;}"), h, fixed = TRUE))
    expect_false(grepl("line-height:35px", h, fixed = TRUE))
    expect_false(grepl("border-radius:35px", h, fixed = TRUE))
})

# spacer.col --------------------------------------------------------------

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

test_that("The corner cell shifts the spacer.col index when show.row.headers is TRUE",
{
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    res <- CreateCustomTable(m4, show.row.headers = TRUE, spacer.col = 2)
    h <- tableHtml(res)
    ths <- regmatches(h, gregexpr('<th class="[^"]*">[^<]*</th>', h))[[1]]
    expect_equal(length(ths), 5)
    # the corner cell occupies emission position 1, so index 2 lands on the first
    # data column ("W") rather than on the corner - pinning the index-shift contract
    expect_equal(ths[1], '<th class="cornerdefault1"></th>')
    expect_equal(ths[2], '<th class="spacer">W</th>')
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

test_that("Out-of-range spacer.col is pinned to its current (defective) behaviour",
{
    # col.header.styles[spacer.col] <- "spacer" silently extends the header-style vector
    # with NA for the skipped positions when spacer.col exceeds its length, and the
    # subsequent sprintf() emits a literal class="NA" <th> rather than erroring or being
    # a no-op. spacer.col = 6 is chosen (a multiple of the 3-column header vector) so the
    # sprintf recycling itself does not error - this pins the actual defect, not a crash.
    x2local <- matrix(1:12, 4, 3, dimnames = list(letters[1:4], c("X", "Y", "Z")))
    res <- CreateCustomTable(x2local, show.row.headers = FALSE, spacer.col = 6)
    h <- tableHtml(res)
    expect_equal(countOccurrences('class="NA"', h), 2)
    expect_true(grepl('<th class="spacer">Z</th>', h, fixed = TRUE))
    expect_equal(countOccurrences("<th ", h), 6)
})

test_that("use.predefined.css = FALSE leaves the spacer header cell with no matching CSS rule",
{
    m4 <- matrix(1:12, 3, 4, dimnames = list(c("a", "b", "c"), c("W", "X", "Y", "Z")))
    res <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 2, use.predefined.css = FALSE)
    h <- tableHtml(res)
    expect_true(grepl('<th class="spacer">', h, fixed = TRUE))
    expect_false(grepl('.spacer {', h, fixed = TRUE))

    # confirm the negative assertion actually flips: with the default use.predefined.css = TRUE
    # the same call does emit the ".spacer {" rule
    resDefault <- CreateCustomTable(m4, show.row.headers = FALSE, spacer.col = 2)
    expect_true(grepl('.spacer {', tableHtml(resDefault), fixed = TRUE))
})

# row.spans --------------------------------------------------------------

# the documented @examples matrix; heights in the tests below are chosen to sum to nrow (4)
# unless the scenario is deliberately exercising mismatched heights
rowSpanMatrix <- structure(1:24, .Dim = c(4L, 6L),
    .Dimnames = list(c("a", "b", "c", "d"), c("A", "B", "C", "D", "E", "F")))

test_that("Default emits no rowspan cells",
{
    res <- CreateCustomTable(x2)
    expect_false(grepl("rowspan=", tableHtml(res), fixed = TRUE))
})

test_that("One rowspan cell per span, with the right heights and labels",
{
    spans <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    res <- CreateCustomTable(rowSpanMatrix, row.spans = spans)
    h <- tableHtml(res)
    # anchored with the trailing quote so rowspan="1" cannot match rowspan="10"-style values
    expect_equal(countOccurrences('<td rowspan="2" class="rowspandefault1">', h), 1)
    expect_equal(countOccurrences('<td rowspan="1" class="rowspandefault1">', h), 2)
    expect_true(grepl('<td rowspan="2" class="rowspandefault1">AA</td>', h, fixed = TRUE))
    expect_true(grepl('<td rowspan="1" class="rowspandefault1">BB</td>', h, fixed = TRUE))
    expect_true(grepl('<td rowspan="1" class="rowspandefault1">CC</td>', h, fixed = TRUE))
})

test_that("Spans are placed at the correct row offsets",
{
    # heights of 2, 1, 1 mean AA's span covers rows 1-2 (leaving row 2 without its own span
    # cell), BB opens row 3 and CC opens row 4 - pinning the j <- j + row.span.lengths[i] loop
    spans <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    res <- CreateCustomTable(rowSpanMatrix, row.spans = spans)
    h <- tableHtml(res)
    body <- sub(".*</thead>", "", h)
    rows <- regmatches(body, gregexpr("<tr>.*?</tr>", body))[[1]]
    expect_equal(length(rows), 4)
    expect_true(grepl('<tr><td rowspan="2"', rows[1], fixed = TRUE))
    expect_false(grepl("rowspan", rows[2], fixed = TRUE))
    expect_true(grepl('<tr><td rowspan="1" class="rowspandefault1">BB</td>', rows[3], fixed = TRUE))
    expect_true(grepl('<tr><td rowspan="1" class="rowspandefault1">CC</td>', rows[4], fixed = TRUE))
})

test_that("A single rowspandefault CSS class is shared across all spans when sticky positioning is off",
{
    # NOT a production bug: addCSSclass() only expands class.css into a per-row/per-column
    # matrix when its `position` argument is non-NULL (see addCSSclass() in
    # createcustomtable.R). Here row.height/num.header.rows are not set, so top.position is
    # NULL and the row-span block's scalar CSS string is never expanded - addCSSclass()
    # derives its class count from length(class.css), so it creates a single
    # "rowspandefault1" class that every span's <td> reuses. (The row-header block passes the
    # same top.position variable as its own `position` argument; on this default path
    # top.position is also NULL, so that block collapses to a single shared class too - it
    # only ends up with one class per row when its call *does* supply a non-NULL `position`,
    # i.e. on the sticky path.) On the sticky path (see the next test) each span DOES
    # get its own "rowspandefaultN" class, matching the plan's expectation.
    spans <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    res <- CreateCustomTable(rowSpanMatrix, row.spans = spans)
    h <- normWs(tableHtml(res))
    # trailing "{" anchors the declaration itself, distinct from the "rowspandefault1" token
    # that also appears (three times, once in each span's class attribute)
    expect_equal(countOccurrences(".rowspandefault1{", h), 1)
    expect_equal(countOccurrences(".rowspandefault2{", h), 0)
    expect_equal(countOccurrences('class="rowspandefault1">', h), 3)
})

test_that("Each span gets its own rowspandefault CSS class when sticky positioning is on",
{
    # With row.height/num.header.rows set, top.position is non-NULL, so addCSSclass()
    # expands the row-span CSS string into one class per span (rowspandefault1/2/3) instead
    # of sharing a single class - the counterpart to the sticky-off case above.
    spans <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    res <- CreateCustomTable(rowSpanMatrix, row.height = "30px", num.header.rows = 1,
                    row.spans = spans)
    h <- normWs(tableHtml(res))
    expect_equal(countOccurrences(".rowspandefault1{", h), 1)
    expect_equal(countOccurrences(".rowspandefault2{", h), 1)
    expect_equal(countOccurrences(".rowspandefault3{", h), 1)
    expect_equal(countOccurrences(".rowspandefault4{", h), 0)
})

test_that("A per-span class is appended, not substituted",
{
    spans <- list(list(height = 2, label = "AA", class = "bluefill"),
                  list(height = 1, label = "BB"), list(height = 1, label = "CC"))
    res <- CreateCustomTable(rowSpanMatrix, row.spans = spans)
    h <- tableHtml(res)
    expect_true(grepl('<td rowspan="2" class="rowspandefault1 bluefill">AA</td>', h, fixed = TRUE))
})

test_that("A span without a class entry carries only the generated class",
{
    spans <- list(list(height = 2, label = "AA", class = "bluefill"),
                  list(height = 1, label = "BB"), list(height = 1, label = "CC"))
    res <- CreateCustomTable(rowSpanMatrix, row.spans = spans)
    h <- tableHtml(res)
    # exact match (no trailing token/space) confirms nothing was appended for BB
    expect_true(grepl('<td rowspan="1" class="rowspandefault1">BB</td>', h, fixed = TRUE))
})

test_that("Span styling arguments reach the rowspandefault CSS declaration",
{
    spans <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    res <- CreateCustomTable(rowSpanMatrix, row.spans = spans, row.span.fill = "rgb(9,9,9)",
                    row.span.font.size = 21, row.span.align.horizontal = "right")
    h <- normWs(tableHtml(res))
    # anchor to the rowspandefault1 declaration itself, not just anywhere in the document,
    # so the assertions cannot be satisfied by an unrelated CSS rule
    block <- regmatches(h, regexpr(".rowspandefault1[{][^}]*[}]", h))
    expect_true(grepl("background: rgb(9,9,9)", block, fixed = TRUE))
    expect_true(grepl("font-size: 21px", block, fixed = TRUE))
    expect_true(grepl("text-align: right", block, fixed = TRUE))
})

test_that("row.spans prepends an extra header cell (ncol + 2 th cells)",
{
    spans <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    # a distinct corner label lets ths[1] (row-span corner, always blank) and ths[2]
    # (row-header corner, carries `corner`) be told apart even if their order were swapped
    res <- CreateCustomTable(rowSpanMatrix, row.spans = spans, corner = "RH")
    h <- tableHtml(res)
    thead <- sub("</thead>.*", "", h)
    ths <- regmatches(thead, gregexpr('<th class="[^"]*">[^<]*</th>', thead))[[1]]
    # 6 data columns + row-header corner + row-span corner
    expect_equal(length(ths), 8)
    expect_equal(ths[1], '<th class="cornerdefault1"></th>')
    expect_equal(ths[2], '<th class="cornerdefault1">RH</th>')
})

test_that("Adding row.spans increases (does not reduce) the emitted sticky-position count",
{
    # PLAN DEVIATION: the plan expected the rm.index pruning at the top of the row.spans
    # block to REDUCE the sticky-position count relative to the no-spans case. In practice
    # that pruning trims the top.position vector, which is reassigned and then also fed
    # into the row-span column's OWN addCSSclass() call; the cell/row-header sticky counts
    # (computed earlier in the function) are unaffected. (A table combining row.spans with
    # col.spans would also see the col-span classes affected, since the later column-span
    # block passes the same pruned top.position as its position argument.) Because the
    # row-span column also gets a "position: sticky" class of its own, adding row.spans
    # increases the total sticky count by 1 (for num.header.rows = 1, the minimal
    # combination) rather than decreasing it. Pinning the actual, confirmed behaviour
    # instead of the plan's assumption.
    # NOTE: at num.header.rows = 1, top.position has length 1, so the emitted output is the
    # same either way: for the heights used here (2, 1, 1) rm.index holds only out-of-range
    # indices (>= 2), so `top.position[-rm.index]` is a no-op. If every span height were 1,
    # rm.index would stay NULL and `top.position[-rm.index]` (i.e. `top.position[-NULL]`)
    # would error rather than silently no-op - but that all-heights-1 combination is never
    # exercised at num.header.rows = 1 by this test. See the num.header.rows = 2 case below
    # for the pruning branch actually removing an index.
    spans <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    noSpans <- CreateCustomTable(rowSpanMatrix, row.height = "30px", num.header.rows = 1)
    withSpans <- CreateCustomTable(rowSpanMatrix, row.height = "30px", num.header.rows = 1,
                    row.spans = spans)
    countNoSpans <- countOccurrences("position: sticky", tableHtml(noSpans))
    countWithSpans <- countOccurrences("position: sticky", tableHtml(withSpans))
    expect_equal(countWithSpans, countNoSpans + 1)
})

test_that("The rm.index pruning branch is actually exercised at num.header.rows = 2",
{
    # At num.header.rows = 2, top.position has length 2, so rm.index pruning is only a
    # no-op when no span's offset (height - 1) lands on index 2. A front-loaded span
    # (heights 2, 1, 1) prunes index 2 out of top.position; a back-loaded span
    # (heights 1, 1, 2) does not prune anything, since its offset lands past top.position's
    # length. Confirmed counts: no spans -> noSpans, front-loaded (pruned) -> noSpans + 1,
    # back-loaded (not pruned) -> noSpans + 2. Expressed relative to noSpans (rather than
    # hardcoded absolutes) so an unrelated CSS addition elsewhere doesn't make this brittle.
    frontLoaded <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    backLoaded <- list(list(height = 1, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 2, label = "CC"))
    resNoSpans <- CreateCustomTable(rowSpanMatrix, row.height = "30px", num.header.rows = 2)
    withFront <- CreateCustomTable(rowSpanMatrix, row.height = "30px", num.header.rows = 2,
                    row.spans = frontLoaded)
    withBack <- CreateCustomTable(rowSpanMatrix, row.height = "30px", num.header.rows = 2,
                    row.spans = backLoaded)
    noSpans <- countOccurrences("position: sticky", tableHtml(resNoSpans))
    expect_equal(countOccurrences("position: sticky", tableHtml(withFront)), noSpans + 1)
    expect_equal(countOccurrences("position: sticky", tableHtml(withBack)), noSpans + 2)
})

test_that("Span heights not summing to nrow are pinned to current (unvalidated) behaviour",
{
    # under-covering: heights sum to 2 against nrow = 4, so rows 3-4 simply get no span cell
    spans <- list(list(height = 1, label = "AA"), list(height = 1, label = "BB"))
    res <- CreateCustomTable(rowSpanMatrix, row.spans = spans)
    h <- tableHtml(res)
    body <- sub(".*</thead>", "", h)
    rows <- regmatches(body, gregexpr("<tr>.*?</tr>", body))[[1]]
    expect_equal(length(rows), 4)
    expect_true(grepl('<tr><td rowspan="1" class="rowspandefault1">AA</td>', rows[1], fixed = TRUE))
    expect_true(grepl('<tr><td rowspan="1" class="rowspandefault1">BB</td>', rows[2], fixed = TRUE))
    expect_false(grepl("rowspan", rows[3], fixed = TRUE))
    expect_false(grepl("rowspan", rows[4], fixed = TRUE))

    # over-covering: a single span with a height greater than nrow completes without error;
    # there is no bounds check in the j <- j + row.span.lengths[i] loop, but with only one
    # span the loop makes just one assignment (row.span.html[1] <- ...) before it ends, so
    # `j` only overruns nrow AFTER that final (and only) span - row.span.html never grows
    # beyond its original length and no misalignment occurs here. See the multi-span case
    # below for what happens when a later span's assignment lands past the overrun.
    overSpans <- list(list(height = 10, label = "AA"))
    res <- CreateCustomTable(rowSpanMatrix, row.spans = overSpans)
    h <- tableHtml(res)
    body <- sub(".*</thead>", "", h)
    rows <- regmatches(body, gregexpr("<tr>.*?</tr>", body))[[1]]
    expect_equal(length(rows), 4)
    expect_true(grepl('<tr><td rowspan="10" class="rowspandefault1">AA</td>', rows[1], fixed = TRUE))
    expect_false(grepl("rowspan", rows[2], fixed = TRUE))
    expect_false(grepl("rowspan", rows[3], fixed = TRUE))
    expect_false(grepl("rowspan", rows[4], fixed = TRUE))
})

test_that("A multi-span over-cover silently drops the last span and leaks a cbind warning (genuine defect, not yet ticketed)",
{
    # Genuine production defect being pinned here, not the plan-authoring mistake above:
    # when a span's height pushes `j` (in the `j <- j + row.span.lengths[i]` loop) past
    # nrow, the next assignment to row.span.html[j] silently grows that vector beyond
    # nrows (here to length 12, for the height-1/10/3 spans below) - so CC's span is written
    # past the end of the table and effectively discarded, rather than misaligning rows 3-4
    # (the emitted rowspan attribute keeps its literal, oversized value - e.g. rowspan="10" -
    # and it is the browser's HTML renderer, not this code, that clamps it to the remaining
    # rows, so the table shape itself stays correct). cbind(row.span.html, cell.html) then
    # warns because the result's row count
    # (4, taken from cell.html) is not a multiple of row.span.html's length (12). No ticket
    # has been filed for this yet.
    overSpans <- list(list(height = 1, label = "AA"), list(height = 10, label = "BB"),
                  list(height = 3, label = "CC"))
    expect_warning(
        res <- CreateCustomTable(rowSpanMatrix, row.spans = overSpans),
        "number of rows of result is not a multiple of vector length", fixed = TRUE
    )
    h <- tableHtml(res)
    body <- sub(".*</thead>", "", h)
    rows <- regmatches(body, gregexpr("<tr>.*?</tr>", body))[[1]]
    expect_equal(length(rows), 4)
    expect_true(grepl('<tr><td rowspan="1" class="rowspandefault1">AA</td>', rows[1], fixed = TRUE))
    expect_true(grepl('<tr><td rowspan="10" class="rowspandefault1">BB</td>', rows[2], fixed = TRUE))
    expect_false(grepl("rowspan", rows[3], fixed = TRUE))
    expect_false(grepl("rowspan", rows[4], fixed = TRUE))
})

test_that("All-height-1 row.spans with sticky positioning errors on invalid unary -NULL (genuine unticketed defect)",
{
    # Genuine production crash being pinned here (not yet ticketed): rm.index is only
    # populated for spans taller than one row, so when EVERY span height is 1, rm.index
    # stays NULL, and the pruning expression `top.position[-rm.index]` becomes
    # `top.position[-NULL]`. In base R, unary minus on NULL is invalid ("invalid argument
    # to unary operator"), so the function throws rather than returning the original
    # top.position unchanged. This only happens when sticky positioning is also active
    # (row.height set AND num.header.rows set), since top.position is only computed - and
    # only fed into the row.spans pruning branch - in that case. If this defect is ever
    # fixed, this expect_error should be changed to assert successful output instead.
    allOnes <- list(list(height = 1, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"), list(height = 1, label = "DD"))
    expect_error(
        CreateCustomTable(rowSpanMatrix, row.spans = allOnes, row.height = "30px",
            num.header.rows = 1),
        "invalid argument to unary operator", fixed = TRUE
    )

    # Same all-height-1 spans without sticky positioning: top.position/rm.index pruning is
    # never reached, so this succeeds - the crash is specific to the sticky combination,
    # not to row.spans (or all-height-1 spans) in general.
    expect_error(res <- CreateCustomTable(rowSpanMatrix, row.spans = allOnes), NA)
    expect_true(grepl('<td rowspan="1" class="rowspandefault1">AA</td>', tableHtml(res),
        fixed = TRUE))

    # Mixed heights (2, 1, 1) with the same sticky args: rm.index is populated (the height-2
    # span), so the pruning expression is a normal subscript, not `-NULL`, and this succeeds.
    mixedHeights <- list(list(height = 2, label = "AA"), list(height = 1, label = "BB"),
                  list(height = 1, label = "CC"))
    expect_error(
        CreateCustomTable(rowSpanMatrix, row.spans = mixedHeights, row.height = "30px",
            num.header.rows = 1),
        NA
    )
})

# banded.rows / banded.cols ------------------------------------------------

test_that("Default emits no banding CSS",
{
    res <- CreateCustomTable(x2)
    expect_false(grepl("nth-child", tableHtml(res), fixed = TRUE))
})

test_that("banded.rows = TRUE emits the odd/even row rule with default fills, including the unscoped even clause",
{
    # cata() joins its arguments with a space (its default cat() sep), so the emitted
    # declaration has spaces around the fill value even though the source concatenates
    # 'background-color:' and the fill value as adjacent cata() arguments; normWs
    # collapses runs of whitespace to a single space rather than removing it, so those
    # spaces remain in the string pinned below
    res <- CreateCustomTable(x2, banded.rows = TRUE)
    h <- normWs(tableHtml(res))
    expect_true(grepl(paste0("tbody tr:nth-child(odd){background-color: rgb(250,250,250) ;}",
                             " tr:nth-child(even){background-color: rgb(245,245,245) ;}"), h, fixed = TRUE))

    # the odd rule is scoped to the container selector, but the even clause that follows
    # it is not re-prefixed - pinning the confirmed selector-scoping gap
    containerSel <- regmatches(h, regexpr("[.]custom-table-container-[A-Za-z0-9_-]+", h, perl = TRUE))
    expect_length(containerSel, 1)
    expect_true(grepl(paste0(containerSel, " tbody tr:nth-child(odd)"), h, fixed = TRUE))
    beforeEven <- substr(h, 1, regexpr("tr:nth-child(even)", h, fixed = TRUE) - 1)
    afterOddRule <- sub(".*tr:nth-child\\(odd\\)\\{[^}]*\\}", "", beforeEven)
    expect_false(grepl(containerSel, afterOddRule, fixed = TRUE))
})

test_that("Custom banded.odd.fill and banded.even.fill values are used instead of the defaults",
{
    res <- CreateCustomTable(x2, banded.rows = TRUE,
                banded.odd.fill = "rgb(1,1,1)", banded.even.fill = "rgb(2,2,2)")
    h <- normWs(tableHtml(res))
    expect_true(grepl(paste0("tbody tr:nth-child(odd){background-color: rgb(1,1,1) ;}",
                             " tr:nth-child(even){background-color: rgb(2,2,2) ;}"), h, fixed = TRUE))
    expect_false(grepl("rgb(250,250,250)", h, fixed = TRUE))
    expect_false(grepl("rgb(245,245,245)", h, fixed = TRUE))
})

test_that("banded.rows = TRUE drops the per-cell background from the celldefault CSS",
{
    resBanded <- CreateCustomTable(x2, cell.fill = "rgb(3,3,3)", banded.rows = TRUE)
    hBanded <- normWs(tableHtml(resBanded))
    expect_false(grepl("background: rgb(3,3,3) ;", hBanded, fixed = TRUE))

    # contrast: with banded.rows = FALSE (the only argument that differs), the same
    # cell.fill value does reach the celldefault declaration
    resFlat <- CreateCustomTable(x2, cell.fill = "rgb(3,3,3)", banded.rows = FALSE)
    hFlat <- normWs(tableHtml(resFlat))
    expect_true(grepl("background: rgb(3,3,3) ;", hFlat, fixed = TRUE))
})

test_that("banded.cols = TRUE also suppresses the cell fill and emits the full column rule verbatim",
{
    # pinning the whole emitted column rule, both the 2n+3 clause and its even-selector
    # continuation with its own fill value (see plan resolved question 1). 2n+3 yields
    # {3,5,7,...} (odd) and even yields {2,4,6,...} - these are disjoint, and with the
    # row-header cell occupying nth-child(1), the data columns land on 2 (even), 3 (2n+3),
    # 4 (even), i.e. a correctly alternating band that excludes the header
    # column. This is not a defect; the row/col naming just doesn't match odd/even parity
    # once the header column is accounted for. (One genuine gap this leaves untested: with
    # show.row.headers = FALSE, column 1 falls into neither selector and is left unbanded -
    # not asserted here.)
    res <- CreateCustomTable(x2, cell.fill = "rgb(3,3,3)", banded.cols = TRUE)
    h <- normWs(tableHtml(res))
    expect_true(grepl(paste0("tbody td:nth-child(2n+3){background-color: rgb(250,250,250) ;}",
                             " td:nth-child(even){background-color: rgb(245,245,245) ;}"),
                       h, fixed = TRUE))
    expect_false(grepl("background: rgb(3,3,3) ;", h, fixed = TRUE))

    # mirrors the row-banding scoping check above: the 2n+3 clause is scoped to the
    # container selector, but the even clause that follows it is not re-prefixed
    containerSel <- regmatches(h, regexpr("[.]custom-table-container-[A-Za-z0-9_-]+", h, perl = TRUE))
    expect_length(containerSel, 1)
    expect_true(grepl(paste0(containerSel, " tbody td:nth-child(2n+3)"), h, fixed = TRUE))
    beforeEven <- substr(h, 1, regexpr("td:nth-child(even)", h, fixed = TRUE) - 1)
    afterOddRule <- sub(".*td:nth-child\\(2n[+]3\\)\\{[^}]*\\}", "", beforeEven)
    expect_false(grepl(containerSel, afterOddRule, fixed = TRUE))
})

test_that("banded.rows and banded.cols together emit both the row and column banding rules",
{
    res <- CreateCustomTable(x2, banded.rows = TRUE, banded.cols = TRUE)
    h <- normWs(tableHtml(res))
    expect_true(grepl("tr:nth-child(odd)", h, fixed = TRUE))
    expect_true(grepl("td:nth-child(2n+3)", h, fixed = TRUE))
})

test_that("Row count does not change how many times the banding rule is emitted",
{
    m2 <- matrix(1:2, 2, 1, dimnames = list(c("a", "b"), "X"))
    m7 <- matrix(1:7, 7, 1, dimnames = list(letters[1:7], "X"))
    res2 <- CreateCustomTable(m2, banded.rows = TRUE)
    res7 <- CreateCustomTable(m7, banded.rows = TRUE)
    expect_equal(countOccurrences("nth-child(odd)", tableHtml(res2)), 1)
    expect_equal(countOccurrences("nth-child(odd)", tableHtml(res7)), 1)
})

# header visibility and cell formatting ----------------------------------

test_that("transpose swaps which labels become row vs column headers",
{
    m <- matrix(1:6, 2, 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
    res <- CreateCustomTable(m, transpose = TRUE)
    h <- normWs(tableHtml(res))

    # the former row names (r1, r2) now form the column-header row
    expect_true(grepl('<th class="colheaderdefault1 ">r1</th><th class="colheaderdefault1 ">r2</th>',
                       h, fixed = TRUE))
    # the former column names (c1, c2, c3) now form the row-header cells
    expect_true(grepl('<td class="rowheaderdefault1">c1</td>', h, fixed = TRUE))
    expect_true(grepl('<td class="rowheaderdefault1">c2</td>', h, fixed = TRUE))
    expect_true(grepl('<td class="rowheaderdefault1">c3</td>', h, fixed = TRUE))
})

test_that("transpose changes the cell order to that of the original first column",
{
    m <- matrix(1:6, 2, 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
    res <- CreateCustomTable(m, transpose = TRUE)
    h <- tableHtml(res)

    # original column c1 was r1=1, r2=2; after transpose this becomes the first body row
    rows <- regmatches(h, gregexpr("<tr>.*?</tr>", h))[[1]]
    firstBodyRow <- rows[2]  # rows[1] is the header row inside <thead>
    expect_true(grepl(">1</td>", firstBodyRow, fixed = TRUE))
    expect_true(grepl(">2</td>", firstBodyRow, fixed = TRUE))
    expect_false(grepl(">5</td>", firstBodyRow, fixed = TRUE))
})

test_that("show.col.headers = FALSE emits no column-header <th> cell and no colheaderdefault CSS rule",
{
    # anchored on 'class="' so the surrounding <thead> tag (which also starts '<th')
    # cannot produce a false positive
    res <- CreateCustomTable(x2, show.col.headers = FALSE)
    h <- tableHtml(res)
    expect_false(grepl('<th class="', h, fixed = TRUE))
    expect_false(grepl("colheaderdefault", h, fixed = TRUE))

    # positive control: both are present when column headers are shown
    resShown <- CreateCustomTable(x2)
    hShown <- tableHtml(resShown)
    expect_true(grepl('<th class="', hShown, fixed = TRUE))
    expect_true(grepl("colheaderdefault", hShown, fixed = TRUE))
})

test_that("show.row.headers = FALSE drops the row-label column and the rowheaderdefault CSS rule",
{
    res <- CreateCustomTable(x2, show.row.headers = FALSE)
    h <- tableHtml(res)
    expect_false(grepl(">a</td>", h, fixed = TRUE))
    expect_false(grepl("rowheaderdefault", h, fixed = TRUE))

    # positive control: both are present when row headers are shown
    resShown <- CreateCustomTable(x2)
    hShown <- tableHtml(resShown)
    expect_true(grepl(">a</td>", hShown, fixed = TRUE))
    expect_true(grepl("rowheaderdefault", hShown, fixed = TRUE))
})

test_that("show.col.headers and show.row.headers both FALSE emits a bare data grid",
{
    res <- CreateCustomTable(x2, show.col.headers = FALSE, show.row.headers = FALSE)
    h <- tableHtml(res)
    expect_false(grepl('<th class="', h, fixed = TRUE))
    expect_false(grepl(">a</td>", h, fixed = TRUE))

    rows <- regmatches(h, gregexpr("<tr>.*?</tr>", h))[[1]]
    firstBodyRow <- rows[1]
    expect_equal(countOccurrences("<td", firstBodyRow), ncol(x2))
})

test_that("NULL rownames force-disable row headers even when show.row.headers = TRUE",
{
    m <- x2
    rownames(m) <- NULL
    res <- CreateCustomTable(m, show.row.headers = TRUE)
    h <- tableHtml(res)
    expect_false(grepl("rowheaderdefault", h, fixed = TRUE))
})

test_that("NULL colnames force-disable column headers even when show.col.headers = TRUE",
{
    m <- x2
    colnames(m) <- NULL
    res <- CreateCustomTable(m, show.col.headers = TRUE)
    h <- tableHtml(res)
    expect_false(grepl("colheaderdefault", h, fixed = TRUE))
})

test_that("cell.align.horizontal reaches text-align and the padding side in the celldefault rule",
{
    res <- CreateCustomTable(x2, cell.align.horizontal = "left")
    h <- normWs(tableHtml(res))
    rule <- regmatches(h, regexpr('\\.celldefault1\\{[^}]*\\}', h))
    expect_length(rule, 1)
    expect_equal(rule, paste0('.celldefault1{ background: #FFFFFF ;; border: 1px solid #FFFFFF;',
        'padding-left:0px; font-size: 13px; font-style: normal; font-weight: normal; ',
        'font-family: Arial; color:#2C2C2C; text-align: left; vertical-align: middle; }'))
})

test_that("font.size propagates to the celldefault, colheaderdefault and rowheaderdefault rules",
{
    res <- CreateCustomTable(x2, font.size = 21)
    h <- normWs(tableHtml(res))

    cellRule <- regmatches(h, regexpr('\\.celldefault1\\{[^}]*\\}', h))
    expect_length(cellRule, 1)
    expect_equal(cellRule, paste0('.celldefault1{ background: #FFFFFF ;; border: 1px solid #FFFFFF;;',
        ' font-size: 21px; font-style: normal; font-weight: normal; font-family: Arial;',
        ' color:#2C2C2C; text-align: center; vertical-align: middle; }'))

    colHdrRule <- regmatches(h, regexpr('\\.colheaderdefault1\\{[^}]*\\}', h))
    expect_length(colHdrRule, 1)
    expect_equal(colHdrRule, paste0('.colheaderdefault1{ background: transparent; height: 35px; ;',
        ' border: 1px solid #FFFFFF;; font-size: 21px; font-style: normal; font-weight: bold;',
        ' font-family: Arial; color:#2C2C2C; text-align: center; vertical-align: middle; }'))

    rowHdrRule <- regmatches(h, regexpr('\\.rowheaderdefault1\\{[^}]*\\}', h))
    expect_length(rowHdrRule, 1)
    expect_equal(rowHdrRule, paste0('.rowheaderdefault1{ background: transparent; border: 1px solid #FFFFFF;',
        'padding-left:0px; font-size: 21px; font-style: normal; font-weight: bold; font-family: Arial;',
        ' color:#2C2C2C; text-align: left; vertical-align: middle; }'))
})

test_that("font.unit is honoured in the emitted font-size declaration",
{
    res <- CreateCustomTable(x2, font.size = 2, font.unit = "em")
    h <- normWs(tableHtml(res))
    expect_true(grepl("font-size: 2em", h, fixed = TRUE))
    expect_false(grepl("font-size: 2px", h, fixed = TRUE))
})

test_that("An explicit cell.font.size overrides font.size for cells only",
{
    res <- CreateCustomTable(x2, font.size = 13, cell.font.size = 30)
    h <- normWs(tableHtml(res))

    cellRule <- regmatches(h, regexpr('\\.celldefault1\\{[^}]*\\}', h))
    expect_length(cellRule, 1)
    expect_true(grepl("font-size: 30px", cellRule, fixed = TRUE))

    colHdrRule <- regmatches(h, regexpr('\\.colheaderdefault1\\{[^}]*\\}', h))
    expect_length(colHdrRule, 1)
    expect_true(grepl("font-size: 13px", colHdrRule, fixed = TRUE))
})

test_that("col.header.classes is appended to the generated colheaderdefault class",
{
    res <- CreateCustomTable(x2, col.header.classes = "myhdr")
    h <- tableHtml(res)
    thTags <- regmatches(h, gregexpr('<th class="[^"]*">[^<]*</th>', h))[[1]]
    dataHeaders <- thTags[grepl("colheaderdefault", thTags, fixed = TRUE)]
    expect_equal(length(dataHeaders), ncol(x2))
    expect_true(all(grepl('colheaderdefault1 myhdr">', dataHeaders, fixed = TRUE)))
})

test_that("row.header.classes is appended to the generated rowheaderdefault class",
{
    res <- CreateCustomTable(x2, row.header.classes = "myrowhdr")
    h <- tableHtml(res)
    rowHdrTags <- regmatches(h, gregexpr('<td class="rowheaderdefault[^"]*">[a-d]</td>', h))[[1]]
    expect_equal(length(rowHdrTags), nrow(x2))
    expect_true(all(grepl('rowheaderdefault1 myrowhdr">', rowHdrTags, fixed = TRUE)))
})

test_that("col.classes applies to a whole data column, indexed against data columns only",
{
    res <- CreateCustomTable(x2, col.classes = list(list(ix = 3, class = "bluefill")),
                              show.row.headers = TRUE)
    h <- tableHtml(res)
    rows <- regmatches(h, gregexpr("<tr>.*?</tr>", h))[[1]]
    bodyRows <- rows[-1]
    expect_equal(length(bodyRows), nrow(x2))
    for (row in bodyRows)
    {
        tds <- regmatches(row, gregexpr('<td class="[^"]*">', row))[[1]]
        # tds[1] is the row-header cell, tds[2:4] are data columns 1:3
        expect_false(grepl("bluefill", tds[2], fixed = TRUE))
        expect_false(grepl("bluefill", tds[3], fixed = TRUE))
        expect_true(grepl("bluefill", tds[4], fixed = TRUE))
    }
})

test_that("row.classes applies to a whole data row",
{
    res <- CreateCustomTable(x2, row.classes = list(list(ix = 1, class = "redfill")))
    h <- tableHtml(res)
    rows <- regmatches(h, gregexpr("<tr>.*?</tr>", h))[[1]]
    bodyRows <- rows[-1]

    row1Tds <- regmatches(bodyRows[1], gregexpr('<td class="[^"]*">', bodyRows[1]))[[1]]
    expect_true(all(grepl("redfill", row1Tds[2:4], fixed = TRUE)))

    row2Tds <- regmatches(bodyRows[2], gregexpr('<td class="[^"]*">', bodyRows[2]))[[1]]
    expect_false(any(grepl("redfill", row2Tds[2:4], fixed = TRUE)))
})

test_that("col.classes and row.classes intersect on the shared cell",
{
    res <- CreateCustomTable(x2, col.classes = list(list(ix = 2, class = "bluefill")),
                              row.classes = list(list(ix = 1, class = "redfill")))
    h <- tableHtml(res)
    rows <- regmatches(h, gregexpr("<tr>.*?</tr>", h))[[1]]
    bodyRows <- rows[-1]

    row1Tds <- regmatches(bodyRows[1], gregexpr('<td class="[^"]*">', bodyRows[1]))[[1]]
    # data column 2 of row 1 carries both classes; columns 1 and 3 of row 1 carry only redfill
    expect_true(grepl("redfill", row1Tds[2], fixed = TRUE))
    expect_false(grepl("bluefill", row1Tds[2], fixed = TRUE))
    expect_true(grepl("redfill", row1Tds[3], fixed = TRUE))
    expect_true(grepl("bluefill", row1Tds[3], fixed = TRUE))
    expect_true(grepl("redfill", row1Tds[4], fixed = TRUE))
    expect_false(grepl("bluefill", row1Tds[4], fixed = TRUE))

    row2Tds <- regmatches(bodyRows[2], gregexpr('<td class="[^"]*">', bodyRows[2]))[[1]]
    expect_false(grepl("redfill", row2Tds[3], fixed = TRUE))
    expect_true(grepl("bluefill", row2Tds[3], fixed = TRUE))
})

test_that("col.classes/row.classes read ix/class positionally, so the element names are cosmetic",
{
    resNamed <- CreateCustomTable(x2, col.classes = list(list(ix = 3, class = "bluefill")))
    resUnnamed <- CreateCustomTable(x2, col.classes = list(list(foo = 3, bar = "bluefill")))

    # the random per-call container name means the documents differ byte-for-byte;
    # compare only the column-3 body cells, which is what this scenario is about
    extractCol3 <- function(h) regmatches(h, gregexpr('<td class="[^"]*">(9|10|11|12)</td>', h))[[1]]
    col3Named <- extractCol3(tableHtml(resNamed))
    col3Unnamed <- extractCol3(tableHtml(resUnnamed))
    expect_length(col3Named, nrow(x2))
    expect_identical(col3Named, col3Unnamed)
})

test_that("The cell.inline.styl typo means sig.change.fills styling is never prepended for the header row",
{
    # createcustomtable.R assigns 'cell.inline.styl <- rbind("", cell.inline.style)' (missing
    # the letter 'e'); this writes to a variable that is never read again, so the header-row
    # blank-style prepend it was meant to add to cell.inline.style is silently discarded. This
    # is a confirmed defect (separate from this test-writing task) - pinned here as current,
    # no-op behaviour: the inline style block is emitted exactly once, on the flagged data cell,
    # never on a header cell.
    sig <- matrix(0, nrow(x2), ncol(x2))
    sig[1, 1] <- 1
    res <- CreateCustomTable(x2, sig.change.fills = sig, show.col.headers = TRUE)
    h <- tableHtml(res)
    expect_equal(countOccurrences("style='background:", h), 1)
    expect_true(grepl("celldefault1\" style='background:rgb(195,255,199)'>1</td>", h, fixed = TRUE))

    headerBlock <- regmatches(h, regexpr("<thead>.*?</thead>", h))
    expect_length(headerBlock, 1)
    expect_false(grepl("style=", headerBlock, fixed = TRUE))
})
