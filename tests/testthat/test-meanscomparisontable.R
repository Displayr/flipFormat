context("CompareMeans")

test_that("Print", {
    library(flipAnalysisOfVariance)
    data(cola, package = "flipExampleData")
    CompareMultipleMeans(list(cola$Q4_A, cola$Q4_B, cola$Q4_C), cola$Q3, compare = "Columns")
    # Weights.
    CompareMultipleMeans(list(cola$Q4_A, cola$Q4_B, cola$Q4_C), cola$Q3, weights = runif(length(cola$Q3)), compare = "Columns")

})

