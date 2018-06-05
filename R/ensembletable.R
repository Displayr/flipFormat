#' Creates a pretty formattable table for an ensemble.
#'
#' @param values A matrix of values.
#' @param order.values Specifies whether the values in the table are sorted by the first column.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @export
EnsembleTable <- function(values,
                          order.values = FALSE,
                          title = "",
                          subtitle = "",
                          footer = "")
{
    column.labels <- colnames(values)
    table.df <- data.frame(vv = values, check.names = FALSE)
    if (order.values)
        table.df <- table.df[order(table.df[, 1], decreasing = TRUE), , drop = FALSE]
    colnames(table.df) <- sprintf("V%d", 1:ncol(table.df))
    formatters <- list()
    for (i in 1:ncol(table.df))
        formatters[[paste0("V", i)]] <- createBarFormatter(bar.shows.magnitude = TRUE)
    createTable(table.df, column.labels, formatters, title, subtitle, footer)
}
