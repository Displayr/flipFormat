context("Sample description")

data(bank, package = "flipExampleData")

test_that("BaseDescription",
          {
              library(flipRegression)
              # Unweighted, unfiltered
              z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
              #z$sample.description
              # Weighted
              z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = ID))
              #z$sample.description
              # Filtered
              z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = ID > 0.5))
              #z$sample.description
              # Weighted, Filtered
              z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = ID))
              #z$sample.description
          })


test_that("No error if all data filtered (n.subset is 0)",
         {
            expect_warning(SampleDescription(n.total = 0, n.subset = 0, n.estimation = 0,
                              "lab", weighted = FALSE, missing = TRUE, imputation.label = "",
                              m = 1, ""))
         })

