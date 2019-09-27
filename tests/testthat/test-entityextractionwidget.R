context("EntityExtractionWidget")

entity.percentages <- c(`Entity 1` = 0.45, `Entity 2` = 0.21, `Entity 3` = 0.78)
variant.percentages <- list(`Entity 1` = c(e1v1 = 0.2, e1v2 = 0.1, e1v3 = 0.15),
                            `Entity 2` = c(e2v1 = 0.21),
                            `Entity 3` = c(e3v1 = 0.08, e3v2 = 0.2, e3v3 = 0.1, e3v4 = 0.3))

test_that("Raw text diagnostics",
{
    result <- EntityExtractionWidget(entity.percentages, variant.percentages,
                                     "Title here", "Footer here")
})

