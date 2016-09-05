context("labels")


test_that("Single case",
          {
              x <- 1
              Labels(x) <- "dog"
              expect_equal(Labels(x), "dog")
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


          })
