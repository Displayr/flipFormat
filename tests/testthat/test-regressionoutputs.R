context("Outputs")
data(bank, package = "flipExampleData")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"
    bank$dep <- (unclass(bank$Overall) - 1) / 6
attr(bank$dep, "label") <- "Overall satisfaction"
attr(bank$Fees, "label") <- "Fees paid"
attr(bank$Online, "label") <- "Online banking"

lapply(bank, function(x) attr(x, "label"))


test_that("Counts are required for count models",{
 #   expect_that(
    z <- GetLabels(names(bank), bank)
    expect_equal(z[3], "Fees paid")
    #Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL)

#    expect_that(Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type= "NBD"), throws_error())
#    expect_that(Regression(dep ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type= "Quasi-Poisson"), throws_error())
})
