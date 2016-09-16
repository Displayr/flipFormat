context("labels")
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
bank$fBranch <- factor(bank$Branch)
attr(bank$fBranch, "label") <- "Branch as a factor"
attr(bank$Overall, "label") <- "Overall satisfaction"


test_that("Single case",
          {
              x <- 1
              Labels(x) <- "dog"
              expect_equal(Labels(x), "dog")
          })

test_that("Fall backs",
{
    x <- 1:100
    expect_equal(Labels(x), "x")
    attr(x, "question") <- "A"
    expect_equal(Labels(x), "A")
    attr(x, "name") <- "B"
    expect_equal(Labels(x), "B")
    attr(x, "label") <- "C"
    expect_equal(Labels(x), "C")
})

test_that("Labels",
{
    data("phone", package = "flipExampleData")
    # A data frame with labels for everything
    expect_equal(as.character(Labels(phone, names(phone))), as.character(unlist(sapply(phone, function(x) attr(x, "label")))))
    # A few missing labels
    phone <- phone[,1:6]
    attr(phone[, 1], "label") <- NULL
    attr(phone[, 3], "label") <- NULL
    expect_equal(Labels(phone, names(phone)), c("id", "Does respondent have a mobile phone?", "q2", "Occupation", "Age",  "Top of mind awareness"))
    # Backticks put in manually.
    names(phone) <- paste0("`", names(phone), "`")
    expect_equal(Labels(phone, names(phone)), c("id", "Does respondent have a mobile phone?", "q2", "Occupation", "Age",  "Top of mind awareness"))
    #Factors in regression models
    data("cola", package = "flipExampleData")
    factor.coefficient.names <- suppressWarnings(names(coef(lm(Q2 ~ Q3, data = cola))))
    expect_equal(Labels(cola, factor.coefficient.names), c("(Intercept)", "Q3. Age: 25 to 29", "Q3. Age: 30 to 34", "Q3. Age: 35 to 39", "Q3. Age: 40 to 44", "Q3. Age: 45 to 49", "Q3. Age: 50 to 54", "Q3. Age: 55 to 64", "Q3. Age: 65 or more"))
    # Regression.
    data("bank", package = "flipExampleData")
    zbank <- bank[1:200,]
    set.seed(23442)
    zbank$rnd <- runif(nrow(zbank))
    zbank$rnd1 <- runif(nrow(zbank))
    zbank$rnd2 <- runif(nrow(zbank))
    zbank$rnd3 <- runif(nrow(zbank))
    zbank$rnd4 <- runif(nrow(zbank))
    attr(bank$Overall, "label") <- "Overall satisfaction"
    attr(bank$Fees, "label") <- "Fees paid"
    attr(bank$Online, "label") <- "Online banking"

    data("cola", package = "flipExampleData")
    cola <- cola[1:150,]
    cola$Q3[1:100] <- NA
    cola$Q3 <- unclass(cola$Q3)
    library(flipRegression)
    Regression(Overall ~ Fees, data = bank, type = "Ordered Logit", missing = "Multiple imputation", detail = FALSE, show.labels = TRUE)
    # Some variables have labels and others do not.
    z <- data.frame(a = 1:10, b = 1:10, c = 1:10)
    Labels(z) <- c("A", "B")
    expect_equal(as.character(Labels(z)), c("A","B", "c"))
})


test_that("Numeric dependent ~ numeric, factor, numeric factor",
          {
              data(colas, package= "flipExampleData")
              library(flipRegression)
              colas$num <- colas$q1a
              colas$q1b <- as.numeric(unclass(colas$q1a))
              colas$q1c <- as.numeric(unclass(colas$q1c))
              z <-suppressWarnings(Regression(num ~ q1b + q3 + q1c + d3, data = colas, detail = FALSE, show.labels = TRUE))
              expect_equal( rownames(z$summary$coefficients)[11], "Gender: Female")
          })

test_that("Regression: labels are extracted from variables containinging $",
          {
              library(flipRegression)
              attach(bank)
              z = data.frame(q = Fees)
              zz <- rownames(Regression(Overall ~ z$q + Phone, detail = FALSE, show.labels = TRUE)$summary$coef)[2]
              expect_equal(zz, "Fees paid")
              detach(bank)
          })


test_that("Regression: Variable names to labels",
{

    # Variable names
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = FALSE))
    expect_equal(rownames(z$summary$coefficients)[5], "fBranch2")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Binary Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = FALSE))
    expect_equal(rownames(z$summary$coefficients)[5], "fBranch2")
    expect_equal(rownames(z$summary$coefficients)[2], "Fees")

    # Variable labels
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Binary Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Multinomial Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(colnames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(colnames(z$summary$coefficients)[5], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Ordered Logit", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[1], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[4], "Branch as a factor: 2")
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Poisson", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")

    # Small binary logit
    data(cola, package = "flipExampleData")
    attr(cola$Q2, "label") <- "Gender"
    attr(cola$Q3, "label") <- "Age of person"
    z <- suppressWarnings(Regression(Q3 ~ Q2, data = cola, type = "Binary Logit", detail = FALSE, show.labels = TRUE))
    expect_equal(rownames(z$summary$coefficients)[2], "Gender: Female")

    # Multiple imputation
    z <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + fBranch + Online + ATM, data = bank, type = "Linear", subset = sb, weights = wgt, detail = FALSE, show.labels = TRUE, missing = "Multiple imputation"))
    expect_equal(rownames(z$summary$coefficients)[2], "Fees paid")
    expect_equal(rownames(z$summary$coefficients)[5], "Branch as a factor: 2")
})









