context("Outputs")
data(bank, package = "flipExampleData")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
    bank$dep <- (unclass(bank$Overall) - 1) / 6
attr(bank$dep, "label") <- "Overall satisfaction"
attr(bank$Online, "label") <- "Online banking"
bank$Fees <- factor(bank$Fees)
levels(bank$Fees)[7] <- levels(bank$Fees)[6]
attr(bank$Fees, "label") <- "Fees paid"

#data(colas, package = "flipExampleData")
#colas$n <- unclass(colas$Q3)
#Regression(colas$Q2~colas$n, type = "Binary Logit")

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
