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


test_that("data frame",
    {
        npk2 <- within(npk, foo <- rnorm(24))
        npk2.aov <- manova(cbind(yield, foo) ~ block, npk2)
        dog <- npk2$yield
        attr(dog, "question") <- "Soloman"
        expect_equal(as.character(Labels(data.frame(dog, npk2$foo))), c("Soloman", "npk2.foo"))
})


test_that("Single case",
          {
              x <- 1
              attr(x, "name") <- "df"
              Labels(x) <- "dog"
              expect_equal(Labels(x), "dog")
              ### Commented out as only fails in testthat
              #expect_equal(Labels(x, show.name = TRUE), "dog (x)")
          })

test_that("Various properties of a label",
          {
              x <- 1
              attr(x, "name") <- "The Name"
              attr(x, "label") <- "The Label"
              attr(x, "question") <- "The Question"
              expect_equal(Labels(x), "The Question: The Label")
              x <- 1
              attr(x, "name") <- "The Name"
              attr(x, "label") <- "The Label"
              expect_equal(Labels(x), "The Label")
              x <- 1
              attr(x, "name") <- "The Name"
              expect_equal(Labels(x), "The Name")
              x <- 1
              attr(x, "name") <- "The Name"
              attr(x, "question") <- "The Question"
              expect_equal(Labels(x), "The Question")
              x <- 1
              attr(x, "label") <- "The Label"
              expect_equal(Labels(x), "The Label")
              x <- 1
              attr(x, "question") <- "The Question"
              expect_equal(Labels(x), "The Question")
              x <- 1
              attr(x, "name") <- "q1"
              attr(x, "label") <- "Q1. The label"
              attr(x, "question") <- "Q1"
              expect_equal(Labels(x), "Q1. The label")
              expect_equal(Labels(x, show.name = TRUE), "Q1. The label (q1)")

          })

test_that("Fall backs",
{
    x <- 1:100
    attr(x, "question") <- "A"
    expect_equal(Labels(x), "A")
    attr(x, "name") <- "B"
    expect_equal(Labels(x), "A")
    attr(x, "label") <- "C"
    expect_equal(Labels(x), "A: C")
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




    # Some variables have labels and others do not.
    z <- data.frame(a = 1:10, b = 1:10, c = 1:10)
    Labels(z) <- c("A", "B")
    expect_equal(as.character(Labels(z)), c("A","B", "c"))
    # DS-2703 Data stacking creates a situation when a variable has a label but not a nem
    # Return the label when name doesnt exist but Labels asks for it (flipRegression does this)
    z <- c(1:2)
    attr(z, "label") <- "C"
    expect_equal(Labels(z, show.name = TRUE), "C")
})






