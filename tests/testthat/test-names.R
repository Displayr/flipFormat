context("names")
test_that("Removal of parent from name", {
    expect_equal(RemoveParentName("x$fdsfds$y"), "y")
    expect_equal(RemoveParentName("fdsfds$y"), "y")
    expect_equal(RemoveParentName("y"), "y")
})


test_that("Removal of parent from name", {
    data(colas, package = "flipExampleData")
    colas <- colas[, 1:12]
    nms <- names(colas)
    expect_equal(Names(colas), nms)
    ### Commented out  because testthat cannot test:
    # expect_equal(Names(colas$q2d), "colas$q2d")
    nms[11] <- attr(colas[, 11], "name") <- "sdffdsfsdfds"
    expect_equal(Names(colas), nms)
    expect_equal(Names(colas$q2c), "sdffdsfsdfds")
})
