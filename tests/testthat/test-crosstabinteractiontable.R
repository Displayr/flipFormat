context("CrosstabInteractionTable")

coef <- array(c(-13.5, -0.3, -86.2,
                -1.3, -25.2, 73.5,
                -2.5, -13.1, -84.3,
                -12.6, -15.1, -72.3),
              dim = 3:4,
              dimnames = list(c("X1", "X2", "X3"), c("a", "b", "c", "NET")))
coef.tstat <- array(c(0, 0.1, -0.2,
                      0, 0, 0.4,
                      0, -0.2, -0.7),
                    dim = c(3L, 3L))
coef.pval <- array(c(1, 0.01, 0.9, 1, 1, 0.7, 1, 0.8, 0.5),
                   dim = c(3L, 3L))


group.size <- array(c(350, 333, 482, 458, 168, 160, 1000, 951),
                    dim = c(2L, 4L), dimnames = list(c("n", "n (after outlier removal)"),
                                                     c("a", "b", "c", "NET")))

group.size.without.outlier.removal <- group.size[1L, ]
group.size.with.outlier.removal <- group.size

test_that("Crosstab Interaction Table", {
    widget <- CrosstabInteractionTable(coef = coef, coef.tstat = coef.tstat,
                                       coef.pval = coef.pval,
                                       group.size = group.size.without.outlier.removal,
                                       footer = "footer here",
                                       title = "Regression table",
                                       subtitle = "subtitle here")

    expect_true(flipChartTests::TestWidget(widget, "crosstab-interaction-no-outlier-widget", height = 800))
    widget <- CrosstabInteractionTable(coef = coef, coef.tstat = coef.tstat,
                                       coef.pval = coef.pval,
                                       group.size = group.size.with.outlier.removal,
                                       footer = "footer here",
                                       title = "Regression table",
                                       subtitle = "subtitle here")
    expect_true(flipChartTests::TestWidget(widget, "crosstab-interaction-outlier-widget", height = 800))
})
