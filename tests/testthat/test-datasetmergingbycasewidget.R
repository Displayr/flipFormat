context("DataSetMergingByCaseWidget")
skip_if_not_installed("flipSnapshotTestUtils")
library(flipSnapshotTestUtils)
library(png)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("merge.data.set.output.rda"))

## merge.output.existing.mergesrc, which is used for testing DS-5301, can be recreated with the
## following code. We include the dput output here to avoid installing/calling flipData
## df1 <- data.frame(x=1:2, mergesrc= as.factor(c('a', 'b')))
## df2 <- data.frame(x=2:3, z = c('a', 'b'))
## file1 <- tempfile(fileext = ".sav")
## file2 <- tempfile(fileext = ".sav")
## outfile <- "foo.sav"  # MergeDataSetsByCase complains if special chars. in filename
## haven::write_sav(df1, file1)
## haven::write_sav(df2, file2)
## result <- MergeDataSetsByCase(c(file1, file2), outfile)
merge.output.existing.mergesrc <- structure(list(input.data.sets.metadata = list(variable.names.list = list(
    file45c053d64f11.sav = c("x", "mergesrc"), file45c01f0a608f.sav = c("x",
    "z")), variable.labels.list = list(file45c053d64f11.sav = c(x = "",
mergesrc = ""), file45c01f0a608f.sav = c(x = "", z = "")), variable.value.attributes.list = list(
    file45c053d64f11.sav = list(x = NULL, mergesrc = c(a = 1,
    b = 2)), file45c01f0a608f.sav = list(x = NULL, z = NULL)),
    variable.types.list = list(file45c053d64f11.sav = c(x = "Numeric",
    mergesrc = "Categorical"), file45c01f0a608f.sav = c(x = "Numeric",
    z = "Text")), n.data.sets = 2L, n.cases = c(file45c053d64f11.sav = 2L,
    file45c01f0a608f.sav = 2L), data.set.names = c("file45c053d64f11.sav",
    "file45c01f0a608f.sav")), merged.data.set.metadata = list(
    variable.names = c("x", "mergesrc", "z"), variable.labels = c(x = "",
    mergesrc = "Source of cases", z = ""), variable.value.attributes = list(
        x = NULL, mergesrc = structure(1:3, names = c("a", "b",
        "file45c01f0a608f.sav")), z = NULL), variable.types = c(x = "Numeric",
    mergesrc = "Categorical", z = "Text"), n.variables = 3L,
    n.cases = 4L, data.set.name = "foo.sav"), matched.names = structure(c("x",
"mergesrc", NA, "x", NA, "z"), dim = 3:2, is.fuzzy.match = structure(c(FALSE,
FALSE, FALSE, FALSE, FALSE, FALSE), dim = 3:2), matched.by = structure(c(NA,
NA, NA, "Variable name", NA, NA), dim = 3:2), match.parameters = list(
    auto.select.what.to.match.by = TRUE, match.by.variable.names = TRUE,
    match.by.variable.labels = FALSE, match.by.value.labels = TRUE,
    ignore.case = TRUE, ignore.non.alphanumeric = TRUE, min.match.percentage = 90,
    min.value.label.match.percentage = 90)), merged.names = structure(c("x",
"mergesrc", "z"), renamed.variables = structure(character(0), dim = c(0L,
2L), dimnames = list(NULL, c("Original name", "New name")))),
    omitted.variable.names.list = list(character(0), character(0)),
    input.value.attributes.list = list(x = NULL, mergesrc = NULL,
        z = NULL), is.saved.to.cloud = FALSE), class = "MergeDataSetByCase")

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

test_that("Pagination and DS-4008", {
    # DS-4008: Q4_B_2 and Q4_C_2 should be highlighted because the code 99 does not appear
    ##  in all input data sets
    widget <- DataSetMergingByCaseWidget(merge.data.set.output$input.data.sets.metadata,
                                         merge.data.set.output$merged.data.set.metadata,
                                         merge.data.set.output$matched.names,
                                         merge.data.set.output$merged.names,
                                         merge.data.set.output$omitted.variable.names.list,
                                         merge.data.set.output$input.value.attributes.list,
                                         merge.data.set.output$is.saved.to.cloud,
                                         page = 2,
                                         variables.per.page = 10)

    if (identical(Sys.getenv("CIRCLECI"), "true"))
    {
        print("Comparing snapshot on Circle-CI")
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

    if (identical(Sys.getenv("CIRCLECI"), "true"))
    {
        print("Comparing snapshot on Circle-CI")
        expect_true(TestWidget(widget, "merging-by-case-widget-warning", height = 800))
    }
})

test_that("DS-5301: Widget creation works if one input dataset already has a mergeSrc variable",
{
    expect_error(print(merge.output.existing.mergesrc), NA)
})
