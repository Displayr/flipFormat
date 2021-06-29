context("DataSetMergingByCaseWidget")

library(flipChartTests)
library(png)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("merge.data.set.output.rda"))

test_that("Data set merging by case widget", {
    widget <- DataSetMergingByCaseWidget(merge.data.set.output$input.data.sets.metadata,
                                         merge.data.set.output$merged.data.set.metadata,
                                         merge.data.set.output$matched.names,
                                         merge.data.set.output$merged.names,
                                         merge.data.set.output$omitted.variable.names,
                                         merge.data.set.output$input.value.attributes,
                                         merge.data.set.output$is.saved.to.cloud)
    expect_true(TestWidget(widget, "merging-by-case-widget", height = 800))
})

