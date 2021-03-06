context("StackingWidget")

library(flipChartTests)
library(png)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("stacking.output.rda"))

test_that("stacking widget", {
    widget <- StackingWidget(stacking.output$input.data.set.metadata,
                             stacking.output$stacked.data.set.metadata,
                             list(c("Last Resp", "Q3")),
                             stacking.output$common.labels,
                             stacking.output$is.saved.to.cloud)
    expect_true(TestWidget(widget, "stacking-widget", height = 3300))
})

