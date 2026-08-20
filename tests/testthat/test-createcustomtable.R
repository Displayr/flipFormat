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

html <- function(res) res$x$text
norm_ws <- function(s) trimws(gsub("\\s+", " ", s))
count_occurrences <- function(pattern, s)
{
    m <- gregexpr(pattern, s, fixed = TRUE)[[1]]
    if (identical(m, -1L)) 0 else length(m)
}

test_that("No sig.leader.circles emits no circle CSS or divs",
{
    res <- CreateCustomTable(x2)
    expect_false(grepl(".circle", html(res), fixed = TRUE))
})

test_that("Base circle classes are emitted when sig.leader.circles is supplied",
{
    circles <- matrix(c(2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0), 4, 3)
    res <- CreateCustomTable(x2, sig.leader.circles = circles)
    h <- norm_ws(html(res))
    expect_true(grepl(".circle2 { border: 2px solid rgb(120,120,120);", h, fixed = TRUE))
    expect_true(grepl(".circle1 { border: 1px solid rgb(150,150,150);", h, fixed = TRUE))
    expect_true(grepl(".circle0 { border: 0px solid rgb(0,0,0);", h, fixed = TRUE))
})

test_that("All nine filled circle variants are emitted with the correct fill colors",
{
    circles <- matrix(c(2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0), 4, 3)
    res <- CreateCustomTable(x2, sig.leader.circles = circles,
                sig.fills.up = "rgb(1,2,3)", sig.fills.nothing = "rgb(4,5,6)",
                sig.fills.down = "rgb(7,8,9)")
    h <- norm_ws(html(res))
    variants <- c("circle21", "circle11", "circle01", "circle20", "circle10",
                  "circle00", "circle2-1", "circle1-1", "circle0-1")
    for (v in variants)
        expect_equal(count_occurrences(paste0(".", v, " {"), h), 1)

    expect_true(grepl(".circle21 { border: 2px solid rgb(120,120,120); background-color:rgb(1,2,3);",
                h, fixed = TRUE))
    expect_true(grepl(".circle10 { border: 1px solid rgb(150,150,150); background-color:rgb(4,5,6);",
                h, fixed = TRUE))
    expect_true(grepl(".circle0-1 { border: 0px solid rgb(0,0,0); background-color:rgb(7,8,9);",
                h, fixed = TRUE))
})

test_that("Every data cell is wrapped in a circle div carrying its own code",
{
    x22 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(2, 1, 0, 2), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- html(res)
    expect_equal(count_occurrences('<div class="circle', h), 4)
    expect_true(grepl('<div class="circle2">1</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle1">2</div>', h, fixed = TRUE))
})

test_that("The rendered cell text survives the circle div wrapping",
{
    x22 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(2, 1, 0, 2), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- html(res)
    expect_true(grepl('<div class="circle0">3</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle2">4</div>', h, fixed = TRUE))
})

test_that("Out-of-range codes pin the current (buggy) normalisation behaviour",
{
    # sig.leader.circles[!which(...)] <- 0 negates integer indices rather than
    # inverting a logical mask, so out-of-range codes are not reset to 0 as documented.
    x22 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("X", "Y")))
    circles <- matrix(c(5, 1, -3, 2), 2, 2)
    res <- CreateCustomTable(x22, sig.leader.circles = circles)
    h <- html(res)
    expect_true(grepl('<div class="circle5">1</div>', h, fixed = TRUE))
    expect_true(grepl('<div class="circle-3">3</div>', h, fixed = TRUE))
})

test_that("circle.size drives the emitted circle geometry",
{
    circles <- matrix(c(2, 1, 0, 2, 1, 0, 2, 1, 0, 2, 1, 0), 4, 3)
    res <- CreateCustomTable(x2, sig.leader.circles = circles, circle.size = 50)
    h <- norm_ws(html(res))
    expect_true(grepl("line-height:50px", h, fixed = TRUE))
    expect_true(grepl("border-radius:50px", h, fixed = TRUE))
    expect_true(grepl("height: 50px", h, fixed = TRUE))
    expect_true(grepl("width:50px", h, fixed = TRUE))
    expect_false(grepl("line-height:35px", h, fixed = TRUE))
    expect_false(grepl("border-radius:35px", h, fixed = TRUE))
})
