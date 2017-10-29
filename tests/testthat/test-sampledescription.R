context("Sample description")


test_that("No error if all data filtered (n.subset is 0)",
         {
            expect_warning(SampleDescription(n.total = 0, n.subset = 0, n.estimation = 0,
                              "lab", weighted = FALSE, missing = TRUE, imputation.label = "",
                              m = 1, ""))
         })

