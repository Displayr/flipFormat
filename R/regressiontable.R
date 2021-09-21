#' RegressionTable
#'
#' Creates a pretty formattable table for regression output.
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
        SE = x ~ FormatAsReal(x, decimals = 2),
        t = createStatisticFormatter("t", "p", p.cutoff),
        p = createPFormatter(p.cutoff)
    )
    createTable(coef.df, c(estimate.name, se.name, statistic.name, p.name), formatters, title, subtitle, footer)
}

#' CrosstabInteractionTable
#'
#' Creates pretty table showing regression coefficients with a crosstab interaction.
#' @param coef Matrix containing estimates of the coefficient.
#' @param coef.tstat Matrix containing t-statistics of each coefficient. Used to determine color of cell.
#' @param coef.pval Matrix containing p-value of each coefficient. Used to determine bolding.
#' @param group.size Vector containing the size of each group (i.e. interaction term).
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param decimals Number of decimals reported for the regression coefficients.
#' @param p.cutoff The alpha level for determining the significance of the coefficients.
#' @importFrom formattable area
#' @importFrom stats setNames
#' @export
CrosstabInteractionTable <- function(coef,
                            coef.tstat,
                            coef.pval,
                            group.size,
                            footer = "",
                            title = "",
                            subtitle = "",
                            decimals = 2,
                            p.cutoff = 0.05)
{
    if (!is.numeric(p.cutoff))
    {
        p.cutoff <- 0.05
        warning("Invalid 'p.cutoff'; 0.05 has been used instead.")
    }
    k <-  ncol(coef) - 1
    coef.names <- colnames(coef)
    colnames(coef.tstat) <- sprintf("t%d", 1:k)
    colnames(coef.pval) <- sprintf("p%d", 1:k)
    coef.df <- data.frame(cbind(coef, coef.tstat, coef.pval), check.names = FALSE)
    if (!is.null(dim(group.size)))
        group.size <- cbind(group.size, array(0, dim = c(2, 2*k), dimnames = list(NULL, colnames(coef.df)[-(1:(k + 1L))])))
    else
        group.size <- rbind(n = c(group.size, setNames(rep(0, 2*k), names(coef.df)[-(1:(k + 1L))])))
    coef.df <- rbind(coef.df, group.size)
    columns.to.exclude <- as.list(structure(rep(FALSE, 2 * k), names=c(colnames(coef.tstat), colnames(coef.pval))))

    formatters <- list()
    for (i in 1:k)
        formatters[[coef.names[i]]] <- createStatisticFormatter(sprintf("t%d",i), sprintf("p%d", i), p.cutoff)
    formatters[["NET"]] <- formatter("span", x~FormatAsReal(x, decimals = decimals))
    formatters[[ncol(coef.df) + 1L]] <- area(row=nrow(coef.df))~formatter("span", x~FormatAsReal(x, decimals = 0))
    if (nrow(group.size) > 1L)
        formatters[[length(formatters) + 1L]] <- area(row=nrow(coef.df) - 1L)~formatter("span", x~FormatAsReal(x, decimals = 0))
    formatters <- c(formatters, columns.to.exclude)

    createTable(coef.df, col.names=coef.names, formatters, title=title, subtitle=subtitle, footer=footer)
}

