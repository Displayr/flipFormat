context("DataSetMergingWidget")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("stacking.output.rda"))

test_that("stacking widget", {
    StackingWidget(stacking.output$stacked.data.set.metadata,
                   list(c("Last Resp", "Q3")),
                   stacking.output$omitted.variables,
                   stacking.output$omitted.stacked.variables,
                   stacking.output$common.labels)
})
