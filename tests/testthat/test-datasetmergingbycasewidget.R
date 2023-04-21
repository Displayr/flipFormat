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
    expect_error(widget <- DataSetMergingByCaseWidget(merge.data.set.output$input.data.sets.metadata,
                                                      merge.data.set.output$merged.data.set.metadata,
                                                      merge.data.set.output$matched.names,
                                                      merge.data.set.output$merged.names,
                                                      merge.data.set.output$omitted.variable.names.list,
                                                      merge.data.set.output$input.value.attributes.list,
                                                      merge.data.set.output$is.saved.to.cloud), NA)

    if (identical(Sys.getenv("TRAVIS"), "true"))
    {
        print("Comparing snapshot on travis")
        expect_true(TestWidget(widget, "merging-by-case-widget", height = 800))
    }
})

test_that("DS-4004: Handles NAs in label matching in valueAttributesTable", {
    # This file is a sanitized snippet of metadata from the file merging
    # error reported in DS-4002, which is the same error from DS-4004.
    # It was produced by adding the following just before the call to
    # valueAttributesTable()

    # if (i == 54) {
    #                 extract.data.for.index <- function(x, inds) {
    #                     x$variable.names.list[[1]] <- x$variable.names.list[[1]][inds[1]]
    #                     x$variable.names.list[[2]] <- x$variable.names.list[[2]][inds[2]]

    #                     x$variable.labels.list[[1]] <- x$variable.labels.list[[1]][inds[1]]
    #                     x$variable.labels.list[[2]] <- x$variable.labels.list[[2]][inds[2]]

    #                     x$variable.value.attributes.list[[1]] <- x$variable.value.attributes.list[[1]][inds[1]]
    #                     x$variable.value.attributes.list[[2]] <- x$variable.value.attributes.list[[2]][inds[2]]

    #                     x$variable.types.list[[1]] <- x$variable.types.list[[1]][inds[1]]
    #                     x$variable.types.list[[2]] <- x$variable.types.list[[2]][inds[2]]
    #                     for (j in seq_along(x)) {
    #                         if (length(x[[j]]) == 2)
    #                             names(x[[j]]) <- c("A", "B")
    #                     }
    #                     x$data.set.names <- c("A", "B")
    #                     x
    #                 }
    #                 tmp.input.data.sets.metadata <- input.data.sets.metadata
    #                 tmp.input.data.sets.metadata <- extract.data.for.index(tmp.input.data.sets.metadata, input.var.ind)
    #                 tmp.input.var.ind <- c(1, 1)
    #                 value.atts.DS4004 <- list(merged.val.attr,
    #                                             tmp.input.data.sets.metadata,
    #                                             input.value.attributes.list[[i]],
    #                                             tmp.input.var.ind)
    #                 names(value.atts.DS4004) <- c("merged.val.attr", "input.data.sets.metadata",
    #                              "input.val.attr.list", "input.var.ind")
    #                 save("value.atts.DS4004", file = "inst/testdata/value.atts.DS4004.rda")
    #             }

    load(findInstDirFile("value.atts.DS4004.rda"))
    expect_error(valueAttributesTable(value.atts.DS4004$merged.val.attr,
                                      value.atts.DS4004$input.data.sets.metadata,
                                      value.atts.DS4004$input.val.attr.list,
                                      value.atts.DS4004$input.var.ind), NA)
})

test_that("Pagination", {
    widget <- DataSetMergingByCaseWidget(merge.data.set.output$input.data.sets.metadata,
                                         merge.data.set.output$merged.data.set.metadata,
                                         merge.data.set.output$matched.names,
                                         merge.data.set.output$merged.names,
                                         merge.data.set.output$omitted.variable.names.list,
                                         merge.data.set.output$input.value.attributes.list,
                                         merge.data.set.output$is.saved.to.cloud,
                                         page = 2,
                                         variables.per.page = 10)

    if (identical(Sys.getenv("TRAVIS"), "true"))
    {
        print("Comparing snapshot on travis")
        expect_true(TestWidget(widget, "merging-by-case-widget-page-2", height = 800))
    }
})

test_that("Too many variables to fit in a page", {
    widget <- DataSetMergingByCaseWidget(merge.data.set.output$input.data.sets.metadata,
                                         merge.data.set.output$merged.data.set.metadata,
                                         merge.data.set.output$matched.names,
                                         merge.data.set.output$merged.names,
                                         merge.data.set.output$omitted.variable.names.list,
                                         merge.data.set.output$input.value.attributes.list,
                                         merge.data.set.output$is.saved.to.cloud,
                                         variables.per.page = 10)

    if (identical(Sys.getenv("TRAVIS"), "true"))
    {
        print("Comparing snapshot on travis")
        expect_true(TestWidget(widget, "merging-by-case-widget-warning", height = 800))
    }
})

