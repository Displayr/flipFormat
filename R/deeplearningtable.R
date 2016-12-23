#' DeepLearningTable
#'
#' Creates a pretty formattable table for variable importance of a deep learning network.
#' @param values A named vector of values
#' @param column.labels Title of column
#' @param order.values Specifies whether the values in the table are sorted
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @references This is based on code written by Kenton Russell.
#' @export
DeepLearningTable <- function(values,
                              column.labels = "Variable Importance",
                              order.values = TRUE,
                              title = "", subtitle = "", footer = "")
{
    table.df <- data.frame(vv = values, check.names = FALSE)
    colnames(table.df) <- sprintf("V%d", 1:ncol(table.df))
    if (order.values)
        table.df <- table.df[order(table.df[,1], decreasing=T),,drop=FALSE]
    formatters <- list()
    formatters[["V1"]] <- createBarFormatter()
    createTable(table.df, column.labels, formatters, title, subtitle, footer)
}
