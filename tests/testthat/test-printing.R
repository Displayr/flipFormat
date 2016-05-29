context("Printing")
test_that("Printing of tables", {

# Functions for printing nicely-formatted tables using DT (data tables HTML widget)
# Testing of the actual appearance of the table should be done in a table test in Q,
# as it does not appear doable in testthat.
test.summary <- structure(c(-2.12354266886328, 0.35430227088273, 0.277320094859762,
                              0.366753235995513, 0.291164750222808, 0.158226380968753, 0.229469583696884,
                              0.375947741901522, 0.0457968464770799, 0.0445643961169206, 0.0464111931711573,
                              0.0379447529295883, 0.042437145474349, 0.0414715092612246, -5.6485049175253,
                              7.73639012590198, 6.22290705190251, 7.90225829021425, 7.67338637737619,
                              3.72848784243494, 5.53318622313647, 4.23783947790291e-08, 2.26073008807065e-13,
                              1.94308676621562e-09, 7.78117175045146e-14, 3.37802828471921e-13,
                              0.000236436041614783, 7.66648022606539e-08), .Dim = c(7L, 4L), .Dimnames = list(
                                  c("(Intercept)", "Fees", "Interest", "Phone", "Branch", "Online",
                                    "ATM"), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"
                                    )))
expect_error(DataTableWithRItemFormat(as.data.frame(test.summary), caption = "A nice summary of regression coefficients.", header.alignments = c("left", "right", "left", "right")), NA)
test.dt <- DataTableWithRItemFormat(as.data.frame(test.summary))
expect_error(AddSignificanceHighlightingToDataTable(test.dt, columns.to.color = "Estimate", column.to.check = "t value", red.value = -1.96, blue.value = 1.96), NA)
})
