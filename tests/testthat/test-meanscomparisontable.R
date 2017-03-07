context("CompareMeans")

test_that("Print", {
    X <- data.frame(means1 = c(NA, 2.94),
                    means2 = c(NA, 3.59),
                    p1 = c(NA, 0.0248),
                    p2 = c(NA, 0.0248),
                    rsquared = c(NA, 0.1),
                    pvalue = c(NA, 0.0248),
                    z1 = c(NA, -2.32),
                    z2 = c(NA, 2.32))
    CNames = LETTERS[1:2]
    means <- X[, 1:2]
    rownames(means) <- c("a", "b")
    zs <- X[, 7:8]
    ps <- X[, 3:4]
    overall.p <- X[, 5]
    r.squared <- X[, 6]

    expect_error(suppressWarnings(MeanComparisonsTable(means, zs, ps, r.squared, overall.p, CNames, "footer", "title", "subtitle", p.cutoff = NULL)), NA)

})
