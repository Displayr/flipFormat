#' AnovaTable
#'
#' Creates a pretty formattable table for ANOVA/ANCOVA of a regression.
#' @param anova An Anova table of Regresssion.
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param p.cutoff The alpha level used when formatting the p-value column.
#' @references This is based on code written by Kenton Russell.
#' @export
AnovaTable <- function(anova,
                            footer,
                            title = "",
                            subtitle = "",
                            p.cutoff = 0.05)
{
    column.names <- colnames(anova)
    if (column.names[1] == "Df")
    {
        column.names <- column.names[c(2, 1, 3)]
        anova <- anova[, c(2, 1, 3)]
    }
    column.names[2] <- "d.f."
    column.names[length(column.names)] <- "<span style='font-style:italic;'>p</span>"
    if (ncol(anova) == 4)
    {   # F-tests
        colnames(anova) <- c("ss", "df", "F", "p")
        formatters <- list(ss = createBarFormatter(),
             df = x ~ FormatAsReal(x, decimals = 0),
             F = createEstimateFormatter("ss", "p", p.cutoff),
             p = createPFormatter(p.cutoff))
        return(createTable(anova, column.names, formatters, title, subtitle, footer))
    }
    # Chi-square tests
    colnames(anova) <- c("chi", "df", "p")
    formatters <- list(chi = createBarFormatter(),
                        df = x ~ FormatAsReal(x, decimals = 0),
                        p = createPFormatter(p.cutoff))
    createTable(anova, column.names, formatters, title, subtitle, footer)
}
