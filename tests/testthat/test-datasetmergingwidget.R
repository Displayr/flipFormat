context("DataSetMergingWidget")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

# load(findInstDirFile("merge.data.set.data.rda")) # delete the file too?
load(findInstDirFile("merge.data.set.output.rda"))

# load(findInstDirFile("merge.data.set.data.2.rda"))

test_that("Data set merging widget", {
    # result <- do.call(DataSetMergingWidget, merge.data.set.data)
    result <- DataSetMergingWidget(merge.data.set.output$input.data.set.metadata,
                                   merge.data.set.output$merged.data.set.metadata,
                                   merge.data.set.output$matched.names,
                                   merge.data.set.output$merged.names,
                                   merge.data.set.output$omitted.variables,
                                   merge.data.set.output$input.value.attributes,
                                   merge.data.set.output$is.saved.to.cloud)
})

# test_that("Data set merging widget with omitted variables", {
#     result <- do.call(DataSetMergingWidget, merge.data.set.data.2)
# })
