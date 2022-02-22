context("CreateCustomTable")

test_that("Percentage data",
{
    xx <- structure(1:5, .Names = c("a", "b", "c", "d", "e"), statistic = "%")
    expect_error(res <- CreateCustomTable(xx), NA)
    expect_equal(attr(res, "ChartData"), xx, check.attributes = FALSE)
    expect_equal(attr(attr(res, "ChartData"), "statistic"), attr(xx, "statistic"))
})
