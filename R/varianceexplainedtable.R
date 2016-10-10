#' VarianceExplainedTable
#'
#' Creates a pretty formattable table for variance explained.
#' @param eigenvalues A vector of eigenvalues corresponding to the components.
#' @param variance.proportions A vector of the variance explained of the components.
#' @param cumulative.proportions A vector of the cumulative variance
#'  explained of the components.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @references This is based on code written by Kenton Russell.
#' @export
VarianceExplainedTable <- function(eigenvalues, variance.proportions, cumulative.proportions,
                                   title = "", subtitle = "", footer = "")
{
    table.df <- data.frame(ev = eigenvalues, v = 100 * variance.proportions,
                           cv = 100 * cumulative.proportions, check.names = FALSE)
    column.labels <- c("Eigenvalue", "% of Variance", "Cumulative %")
    formatters <- list()
    formatters[["ev"]] <- createBarFormatter()
    formatters[["v"]] <- createBarFormatter()
    formatters[["cv"]] <- createBarFormatter()
    createTable(table.df, column.labels, formatters, title, subtitle, footer)
}
