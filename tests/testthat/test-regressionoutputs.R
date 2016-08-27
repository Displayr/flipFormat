context("Outputs")

test_that("Labels using attr(, )",{
 #   expect_that(
   #  library(flipRegression)
   #  suppressWarnings(Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, detail = FALSE))
   #  suppressWarnings(Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL))
   # #z
    #z <- GetLabels(rownames(z$summary$coefficients), bank)
    #expect_equal(z[3], "Fees paid: 2")
#    expect_that(Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type= "NBD"), throws_error())
#    expect_that(Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type= "Quasi-Poisson"), throws_error())
})


#
# # Small binary logit
# library(flipRegression)
# data(colas, package = "flipExampleData")
# attr(colas$Q2, "label") <- "Gender"
# attr(colas$Q3, "label") <- "Age of person"
# Regression(Q3 ~ Q2, data = colas, type = "Binary Logit", detail = FALSE, show.labels = TRUE)
