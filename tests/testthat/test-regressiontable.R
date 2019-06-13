context("RegressionTable")

coefficient.table <- matrix(c(2.3, 3.4, -1.2,
                              1.2, 2.8, 0.5,
                              1.92, 1.21, -2.4,
                              0.055, 0.226, 0.016), ncol = 4)

rownames(coefficient.table) <- c("predictor 1", "predictor 2", "predictor 3")

RegressionTable(coefficient.table = coefficient.table,
                footer = "footer here",
                title = "Regression table",
                subtitle = "subtitle here")
