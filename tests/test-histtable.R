context("HistTable")

dat <- data.frame(A=rpois(500,5), B=rpois(500,50), C=rpois(500,20))
expect_error(HistTable(dat,
                       title = "Histograms",
                       subtitle = "subtitle here",
                       footer = "footer here"), NA)
