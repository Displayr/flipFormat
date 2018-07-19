#' Creates a pretty formattable table for an comapring models.
#'
#' @param values A matrix of values.
#' @param order.values Specifies whether the values in the table are sorted by the first column.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @export
ComparisonTable <- function(values,
                          order.values = FALSE,
                          title = "",
                          subtitle = "",
                          footer = "")
{
    column.labels <- colnames(values)
    table.df <- data.frame(values, check.names = FALSE)

    if (order.values)
        table.df <- table.df[order(table.df[, 1], decreasing = TRUE), , drop = FALSE]

    formatters <- list()

    formatters[["In-sample accuracy"]] <- createBarFormatter(decimals = 2, show.as.percent = TRUE)
    formatters[["Out-sample accuracy"]] <- createBarFormatter(decimals = 2, show.as.percent = TRUE)
    formatters[["BIC"]] <- createBarFormatter(decimals = 0)
    formatters[["Log-likelihood"]] <- createBarFormatter(decimals = 0)
    formatters[["Time taken (s)"]] <- createBarFormatter(decimals = 0)
    # do not need to fomat Classes

    createTable(table.df, column.labels, formatters, title, subtitle, footer)
}
