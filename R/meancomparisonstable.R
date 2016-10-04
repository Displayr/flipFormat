#' MeanComparisonsTable
#'
#' Creates a pretty formattable table for mean comparisons.
#' @param means The means to be shown on the table.
#' @param zs Z-Statistics. Only used to determine the color of the fonts.
#' @param ps P-Values associated with each mean. Used to determine whether or not to color the cells.
#' @param r.squared The r-squared value for the regression predicting the row variable by the columns.
#' @param overall.p The P-value for the regression (e.g., an ANOVA F-Test for a linear model).
#' @param column.names The names to put on the columns of the table (other than for the R-Squared and overall P).
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param p.cutoff The alpha level used when formatting the p-value column.
#' @references This is based on code written by Kenton Russell.
#' @export
MeanComparisonsTable <- function(means, zs, ps, r.squared, overall.p, column.names, footer, title = "", subtitle = "", p.cutoff = 0.05)
{
    colnames(zs) <- paste0(LETTERS[1:ncol(zs)], 2)
    # Putting all the tables into a single data.frame, as required by formattable.
    means <- as.data.frame(cbind(means, ps, rsquared = r.squared, pvalue = overall.p, zs))
    k <- length(column.names) #Number of being compared.
    column.names <- c(column.names, "R-Squared", "<i>p</i>")
    formatters <- list()
    for (i in 1:k)
    {
        l <- LETTERS[i]
        formatters[[l]] <- createHeatmapFormatter(paste0(l, 2), paste0(l, 1), p.cutoff)
    }
    formatters[["rsquared"]] <- createRSquaredFormatter()
    formatters[["pvalue"]] <- createPFormatter(p.cutoff)
    # Removing unwanted variables (i.e., the variables that contain the p-values and z statistics)
    p.values <- rep(FALSE, k)
    names(p.values) <- paste0(LETTERS[1:k], "1")
    z.stats <- rep(FALSE, k)
    names(z.stats) <- paste0(LETTERS[1:k], "2")
    formatters <- c(formatters, as.list(p.values), as.list(z.stats))
    createTable(means, column.names, formatters, title, subtitle, footer)
}
