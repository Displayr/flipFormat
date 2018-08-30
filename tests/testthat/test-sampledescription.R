context("Sample description")


test_that("No error if all data filtered (n.subset is 0)",
         {
            expect_warning(SampleDescription(n.total = 0, n.subset = 0, n.estimation = 0,
                              "lab", weighted = FALSE, missing = TRUE, imputation.label = "",
                              m = 1, ""))
         })

test_that("Effective sample size", {
    expect_equal(SampleDescription(n.total = 302, n.subset = 200,
         n.estimation = 100, "lab",
         weighted = TRUE,
         weight.label = "wgt",
         missing = "Exclude cases with missing data",
         effective.sample.size = 127.07),
         paste0("n = 100 cases used in estimation of a total sample size of 200 (lab); ",
                "data has been weighted (wgt); effective sample size: 127.07; ",
                "cases containing missing values have been excluded;"))})

