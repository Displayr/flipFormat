context("DataSetMergingByVariableWidget")

library(flipSnapshotTestUtils)
library(png)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("merge.data.set.by.var.output.rda"))

test_that("Data set merging by variable widget", {
    expect_error(widget <- DataSetMergingByVariableWidget(merge.data.set.by.var.output$input.data.sets.metadata,
                                                          merge.data.set.by.var.output$merged.data.set.metadata,
                                                          merge.data.set.by.var.output$source.data.set.indices,
                                                          merge.data.set.by.var.output$omitted.variable.names,
                                                          merge.data.set.by.var.output$merged.id.variable.name,
                                                          merge.data.set.by.var.output$id.variable.names,
                                                          merge.data.set.by.var.output$example.id.values,
                                                          merge.data.set.by.var.output$is.saved.to.cloud), NA)

    if (identical(Sys.getenv("TRAVIS"), "true"))
    {
        print("Comparing snapshot on travis")
        expect_true(TestWidget(widget, "merging-by-variable-widget", height = 800))
    }
})

test_that("Pagination", {
    widget <- DataSetMergingByVariableWidget(merge.data.set.by.var.output$input.data.sets.metadata,
                                             merge.data.set.by.var.output$merged.data.set.metadata,
                                             merge.data.set.by.var.output$source.data.set.indices,
                                             merge.data.set.by.var.output$omitted.variable.names,
                                             merge.data.set.by.var.output$merged.id.variable.name,
                                             merge.data.set.by.var.output$id.variable.names,
                                             merge.data.set.by.var.output$example.id.values,
                                             merge.data.set.by.var.output$is.saved.to.cloud,
                                             page = 2,
                                             variables.per.page = 10)

    if (identical(Sys.getenv("TRAVIS"), "true"))
    {
        print("Comparing snapshot on travis")
        expect_true(TestWidget(widget, "merging-by-variable-widget-page-2", height = 800))
    }
})
