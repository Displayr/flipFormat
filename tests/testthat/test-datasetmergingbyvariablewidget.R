context("DataSetMergingByCaseWidget")

library(flipChartTests)
library(png)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("merge.data.set.by.var.output.rda"))

test_that("Data set merging by variable widget", {
    widget <- DataSetMergingByVariableWidget(merge.data.set.by.var.output$input.data.set.metadata,
                                             merge.data.set.by.var.output$merged.data.set.metadata,
                                             merge.data.set.by.var.output$omitted.variables,
                                             merge.data.set.by.var.output$is.saved.to.cloud)
    expect_true(TestWidget(widget, "merging-by-variable-widget", height = 800))
})
