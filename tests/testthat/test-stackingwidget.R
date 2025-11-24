context("StackingWidget")

skip()
library(flipSnapshotTestUtils)
library(png)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("stacking.output.rda"))

test_that("stacking widget", {
    expect_error(widget <- StackingWidget(stacking.output$input.data.set.metadata,
                                          stacking.output$stacked.data.set.metadata,
                                          list(c("Last Resp", "Q3")),
                                          stacking.output$common.labels,
                                          stacking.output$is.saved.to.cloud), NA)

    if (identical(Sys.getenv("TRAVIS"), "true"))
    {
        print("Comparing snapshot on travis")
        expect_true(TestWidget(widget, "stacking-widget", height = 3300))
    }
})

