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

test_that("Missing value strings", {
    n.tot <- 123
    n.sub <- 123
    n.est <- 100
    # Missing data excluded
    expect_equal(SampleDescription(n.total = n.tot, n.subset = n.sub,
                                   weighted = FALSE, n.estimation = n.est,
                                   missing = "Exclude cases with missing data"),
                 paste0("n = ", n.est, " cases used in estimation of a total sample size of ", n.sub, "; ",
                        "cases containing missing values have been excluded;"))
    # Error if missing
    expect_equal(SampleDescription(n.total = n.tot, n.subset = n.sub,
                                   weighted = FALSE, n.estimation = n.est,
                                   missing = "Error if missing data"),
                 paste0("n = ", n.est, " cases used in estimation of a total sample size of ", n.sub, ";"))
    # Single imputation
    expect_equal(SampleDescription(n.total = n.tot, n.subset = n.sub,
                                   weighted = FALSE, n.estimation = n.est,
                                   missing = "Imputation (replace missing values with estimates)",
                                   imputation.label = "chained equations (predictive mean matching)",
                                   variable.description = "predictor"),
                 paste0("n = ", n.est, " cases used in estimation of a total sample size of ", n.sub, "; ",
                        "missing values of predictor variables have been imputed using chained equations ",
                        "(predictive mean matching);"))
    # Multiple imputation
    expect_equal(SampleDescription(n.total = n.tot, n.subset = n.sub,
                                   weighted = FALSE, n.estimation = n.est,
                                   missing = "Multiple imputation", m = 2,
                                   imputation.label = "chained equations (predictive mean matching)",
                                   variable.description = "predictor"),
                 paste0("n = ", n.est, " cases used in estimation of a total sample size of ", n.sub, "; ",
                        "multiple imputation (m = 2, chained equations (predictive mean matching)) has been ",
                        "used to impute missing values of predictor variables;"))
    # Dummy variable adjustment selected but no dummy variables created. No dummy variable message
    # should appear in footer
    expect_equal(SampleDescription(n.total = n.tot, n.subset = n.sub,
                                   weighted = FALSE, n.estimation = n.est,
                                   missing = "Dummy variable adjustment",
                                   dummy.adjusted = FALSE),
                 paste0("n = ", n.est, " cases used in estimation of a total sample size of ", n.sub, ";"))
    # Expect dummy variable adjustment method message to appear
    expect_equal(SampleDescription(n.total = n.tot, n.subset = n.sub,
                                   weighted = FALSE, n.estimation = n.est,
                                   missing = "Dummy variable adjustment",
                                   dummy.adjusted = TRUE),
                 paste0("n = ", n.est, " cases used in estimation of a total sample size of ", n.sub, "; ",
                        "missing values of variables have been adjusted using dummy variables;"))


})


test_that("RS-20042: Rounding not occuring for sample size", {
    SampleDescription(
        n.total = 24332L, n.subset = 1932L, n.estimation = 998L,
        subset.label = "Current and Global Filter", weighted = TRUE,
        weight.label = "Weight_RF: Weight_RF", missing = "Exclude cases with missing data"
    ) |>
        expect_equal(
            paste0(
                "n = 998 cases used in estimation of a total sample size of 1,932 (Current and Global Filter); ",
                "data has been weighted (Weight_RF: Weight_RF); ",
                "cases containing missing values have been excluded;"
            )
        )
})
