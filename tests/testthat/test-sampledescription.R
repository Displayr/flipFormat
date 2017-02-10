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

