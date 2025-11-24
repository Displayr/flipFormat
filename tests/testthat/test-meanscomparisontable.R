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

test_that("Means with no variation are not shaded", {
    means <- array(
        c(1, 1, 0.93, 1, 1, 0.92, 1, 1, 0.9, 1, 0, 0.9),
        dim = c(6L, 2L),
        dimnames = list(
            c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max"),
            c("Cluster 1 82%", "Cluster 2 18%")
        )
    )
    zs <- array(
        c(-2.11, -2.11, 1.3, -2.11, 961230146036560, 0.75, 2.11, 2.11, -1.3, 2.11, -961230146036561, -0.75),
        dim = c(6L, 2L),
        dimnames = list(NULL, c("Cluster 1 82%", "Cluster 2 18%"))
    )
    ps <- array(
        c(0.21, 0.21, 1, 0.21, 0, 1, 0.21, 0.21, 1, 0.21, 0, 1),
        dim = c(6L, 2L),
        dimnames = list(NULL, c("Cluster 1 82%", "Cluster 2 18%"))
    )
    r.squared <- c(NA, NA, 0.0021, NA, 1, 0.0007)
    overall.p <- c(0.2099, 0.2099, 1, 0.2099, 0, 1)
    column.names <- c("Cluster 1\n82%<br>n: 653", "Cluster 2\n18%<br>n: 147")
    footer <- paste0(
        "n = 800 cases used in estimation; null hypotheses: two-sided; ",
        "multiple comparisons correction: False Discovery Rate correction applied simultaneously to the entire table"
    )
    title <- "K-Means: 2 clusters"
    subtitle <- "Variance explained: 50%; Calinski-Harabasz: 804; Profiling: 1 significant (Age)"
    output <- MeanComparisonsTable(means, zs, ps, r.squared, overall.p, column.names, footer, title, subtitle)
    skip_if_not_installed("flipSnapshotTestUtils")
    TestWidget(output, "means-no-variation-no-shading") |> expect_true()
})
