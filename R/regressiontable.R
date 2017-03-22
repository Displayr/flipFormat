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
        SE = x ~ FormatWithDecimals(x, 2),
        t = createHeatmapFormatter("t", "p", p.cutoff),
        p = createPFormatter(p.cutoff)
    )
    createTable(coef.df, c(estimate.name, se.name, statistic.name, p.name), formatters, title, subtitle, footer)
}

#' CrosstabInteractionTable
#'
#' Creates pretty table showing regression coefficients with a crosstab interaction.
#' @param coef Matrix containing estimates of the coefficient.
#' @param coef.sign Matrix of \code{-1,0,1}s indicating whether an interaction term makes a
#'      coefficient significantly smaller (-1) or bigger (+1) than without the interaction.
#' @param group.size Vector containing the size of each group (i.e. interaction term).
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param sign.color Vector of three colors, which controls how coefficient values which are significant will be displayed.
#' @param decimals Number of decimals reported for the regression coefficients.
#' @importFrom formattable area
#' @export
CrosstabInteractionTable <- function(coef,
                            coef.sign,
                            group.size,
                            footer = "",
                            title = "",
                            subtitle = "",
                            sign.color = c("red", "black", "blue"),
                            decimals = 2)
{
    coef.df <- data.frame(coef, check.names = FALSE)
    coef.df <- rbind(coef.df, n = group.size)

    formatters <- list()
    for (i in 1:(ncol(coef.df)-1))
    {
        i.name <- colnames(coef)[i]
        i.col <- c(sign.color[coef.sign[,i]+2], "black")
        formatters[[i.name]] <- formatter("span", style=style(color=i.col), x ~ FormatWithDecimals(x, decimals))
    }
    formatters[["NET"]] <- formatter("span", x~FormatWithDecimals(x, decimals))
    formatters[[ncol(coef.df)+1]] <- area(row=nrow(coef.df))~formatter("span", x~FormatWithDecimals(x,0))

    createTable(coef.df, col.names=colnames(coef.df), formatters, title=title, subtitle=subtitle, footer=footer)
}

