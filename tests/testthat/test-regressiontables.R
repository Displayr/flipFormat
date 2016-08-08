context("Pretty regression tables")

test_that("PrettyRegressionTable",{

    ft <- "Yo! This footer specifically designed
          to communicate important information.
    Since it is so important, it will of course
    extend over many lines.  In fact, on narrow tables,
    it might take >3.  On wide tables, it might only
    require one.  Feel free to adjust the width,
    and the importance and significance does not
    go away."

    data(weight, package = "flipExampleData")
    z = summary(lm(Weight ~ Height + Age, data = weight))$coef
    expect_error(PrettyRegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
    PrettyRegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog")

    ## Linear regression
    data(bank, package = "flipExampleData")
    library(flipRegression)
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, detail = FALSE))

    # Linear regression with robust se
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, robust.se = TRUE, detail = FALSE))

    # Ordered logit (has a z statistic rather than a t)
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, type = "Ordered Logit", detail = FALSE))

    coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
    rownames(coef.matrix)[1] <- "Big dog"
    PrettyRegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog")

    expect_error(PrettyRegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
})
