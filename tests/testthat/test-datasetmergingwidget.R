context("DataSetMergingWidget")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("merge.data.set.data.rda"))
load(findInstDirFile("merge.data.set.data.2.rda"))

test_that("Data set merging widget", {
    result <- do.call(DataSetMergingWidget, merge.data.set.data)
})

test_that("Data set merging widget with omitted variables", {
    result <- do.call(DataSetMergingWidget, merge.data.set.data.2)
})
