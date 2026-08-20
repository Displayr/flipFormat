context("CreateCustomTable")

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

tableHtml <- function(res) res$x$text  # rhtmlMetro::Box stores the emitted HTML in the widget payload's text field
normWs <- function(s) trimws(gsub("\\s+", " ", s))
countOccurrences <- function(pattern, s)
{
    m <- gregexpr(pattern, s, fixed = TRUE)[[1]]
    if (identical(m, -1L)) 0 else length(m)
}

test_that("No sig.leader.circles emits no circle CSS or divs",
{
    res <- CreateCustomTable(x2)
    expect_false(grepl(".circle", tableHtml(res), fixed = TRUE))
})

test_that("Base circle classes are emitted when sig.leader.circles is supplied",
{
    # circle CSS is value-independent (driven only by non-NULL-ness of sig.leader.circles,
    # circle.size and sig.fills.*); a plain fixture is used so the codes don't misleadingly
    # appear to drive the CSS
    circles <- matrix(0, 4, 3)
    res <- CreateCustomTable(x2, sig.leader.circles = circles)
    h <- normWs(tableHtml(res))
    expect_true(grepl(".circle2 { border: 2px solid rgb(120,120,120);", h, fixed = TRUE))
    expect_true(grepl(".circle1 { border: 1px solid rgb(150,150,150);", h, fixed = TRUE))
    expect_true(grepl(".circle0 { border: 0px solid rgb(0,0,0);", h, fixed = TRUE))
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
        # the trailing " {" is load-bearing: a bare ".circle2" also matches ".circle21",
        # ".circle20" and ".circle2-1", so without it the occurrence count would be wrong
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
    expect_equal(countOccurrences('<div class="circle', h), 4)
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
    # RS-21803: sig.leader.circles[!which(...)] <- 0 negates integer indices rather than
    # inverting a logical mask, so out-of-range codes are not reset to 0 as documented.
    # This test pins that behaviour deliberately, pending a fix.
    x22 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(5, 1, -3, 2), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- tableHtml(res)
    expect_true(grepl('<div class="circle5">1</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle-3">3</div>', h, fixed = TRUE))
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
