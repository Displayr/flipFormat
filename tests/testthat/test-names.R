context("names")
test_that("Removal of parent from name", {
    expect_equal(RemoveParentName("x$fdsfds$y"), "y")
    expect_equal(RemoveParentName("fdsfds$y"), "y")
    expect_equal(RemoveParentName("y"), "y")
})
