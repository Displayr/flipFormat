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

    # Data used to lookup formatting according to column label
    columns <- c("In-sample accuracy", "Out-sample accuracy", "In-sample RMSE", "Out-sample RMSE",
                 "In-sample R^2", "Out-sample R^2", "BIC", "Log-likelihood", "Time taken (s)")
    decimal.places <- c(2, 2, 2, 2, 4, 4, 0, 0, 0)
    percents <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

    formatters <- list()

    for (label in column.labels)
    {
        i <- match(label, columns, nomatch = 0)
        if (i > 0) {
            formatters[[label]] <- createBarFormatter(decimals = decimal.places[i],
                                                   show.as.percent = percents[i])
        }
    }

    createTable(table.df, column.labels, formatters, title, subtitle, footer)
}
