


test_that("RegressionTable",{

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
    expect_error(RegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
    RegressionTable(z, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog", p.cutoff = 0.8)

    ## Linear regression
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, detail = FALSE))

    # Linear regression with robust se
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, robust.se = TRUE, detail = FALSE))

    # Ordered logit (has a z statistic rather than a t)
    suppressWarnings(Regression(Overall ~  Fees + Interest + Phone + Branch + Online  +ATM, data = bank, type = "Ordered Logit", detail = FALSE))

    coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
    rownames(coef.matrix)[1] <- "Big dog"
    RegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog")

    expect_error(RegressionTable(coef.matrix, TRUE, footer = ft,  title = "My awesome regression", subtitle = "Big brown dog"), NA)
})






