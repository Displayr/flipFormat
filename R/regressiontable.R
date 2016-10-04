#'
#' Creates a pretty formattable table.
#' @param coefficient.table A table of regression coefficients, standard errors, z or t statistics, and p-values.
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param estimate.name The name of the Estimate column. Defaults to "Estimate".
#' @param se.name The name of the standard error column. Defaults to "Standard<br/>Error".
#' @param statistic.name The name of the test-statistic column. Defaults to "<span style='font-style:italic;'>t</span>"".
#' @param p.name The name of the p-value column. Defalts to "Estimate".
#' @param p.cutoff The alpha level used when formatting the p-value column.
#' @references This is based on code written by Kenton Russell.
#' @export
RegressionTable <- function(coefficient.table,
                            footer,
                            title = "",
                            subtitle = "",
                            estimate.name = "Estimate",
                            se.name = "Standard<br/>Error",
                            statistic.name = "<span style='font-style:italic;'>t</span>",
                            p.name = "<span style='font-style:italic;'>p</span>",
                            p.cutoff = 0.05)
{
    colnames(coefficient.table) <- c("Estimate", "SE", "t", "p")
    coef.df <- data.frame(coefficient.table, check.names = FALSE)
    formatters <- list(
        Estimate = createEstimateFormatter("t", "p", p.cutoff),
        SE = x ~ FormatWithDecimals(x, 2),
        t = createHeatmapFormatter("t", "p", p.cutoff),
        p = createPFormatter(p.cutoff)
    )
    createTable(coef.df, c(estimate.name, se.name, statistic.name, p.name), formatters, title, subtitle, footer)
}
