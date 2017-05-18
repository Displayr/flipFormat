#' MaxDiffTable
#'
#' Creates a pretty formattable table for max-diff output.
#' @param stats.table A matrix of the statistics to be shown in the table.
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @references This is based on code written by Kenton Russell.
#' @export
MaxDiffTable <- function(stats.table,
                         title = "",
                         subtitle = "",
                         footer = "")
{
    colnames(stats.table) <- c("mean", "median", "lower.quartile", "upper.quartile", "min", "max")
    stats.table <- data.frame(stats.table, check.names = FALSE)
    formatters <- list(
        mean = createBarFormatter(decimals = 1),
        median = x ~ FormatWithDecimals(x, 1),
        lower.quartile = x ~ FormatWithDecimals(x, 1),
        upper.quartile = x ~ FormatWithDecimals(x, 1),
        min = x ~ FormatWithDecimals(x, 1),
        max = x ~ FormatWithDecimals(x, 1))
    column.labels <- c("Mean (%)", "Median (%)", "Lower<br>quartile (%)", "Upper<br>quartile (%)", "Min (%)", "Max (%)")
    createTable(100 * stats.table, column.labels, formatters, title, subtitle, footer)
}

#' MaxDiffTableClasses
#'
#' Creates a pretty formattable table for max-diff output.
#' @param probabilities A matrix of the probabilities to be shown in the table.
#' @param column.labels Labels to show in the columns.
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @references This is based on code written by Kenton Russell.
#' @export
MaxDiffTableClasses <- function(probabilities,
                         column.labels,
                         title = "",
                         subtitle = "",
                         footer = "")
{
    k <- ncol(probabilities)
    colnames(probabilities) <- paste0("class", 1:k)
    prob.df <- data.frame(probabilities, check.names = FALSE)
    formatters <- list()
    for (i in 1:k)
        formatters[[paste0("class", i)]] <- createBarFormatter(decimals = 1,
                                                               bar.shows.magnitude = TRUE,
                                                               max.display.value = 100 * max(prob.df))
    createTable(100 * prob.df, column.labels, formatters, title, subtitle, footer)
}
