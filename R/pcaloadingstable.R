#' PCALoadingsTable
#'
#' Creates a pretty formattable table for PCA loadings.
#' @param loadings.matrix A matrix of the PCA loadings/structure.
#' @param variance.explained A vector of the variance explained of the components.
#' @param eigenvalues A vector of the eigenvalues corresponding to the components.
#' @param min.display.value The minimum magnitude necessary for a value to be displayed.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @references This is based on code written by Kenton Russell.
#' @export
PCALoadingsTable <- function(loadings.matrix, variance.explained, eigenvalues,
                             min.display.value, title = "", subtitle = "", footer = "")
{
    k <- ncol(loadings.matrix)
    column.labels <- character(k)
    for (i in 1:k)
    {
        parentheses.text <- if (!is.null(variance.explained))
            paste0(" (", FormatAsPercent(variance.explained[i], 3), ")")
        else
            ""
        column.labels[i] <- paste0(colnames(loadings.matrix)[i], parentheses.text,
                                   lineBreakPlaceholder(), beginSubheadingPlaceholder(), "Eigenvalue: ",
                                   FormatWithDecimals(eigenvalues[i], 2), endSubheadingPlaceholder())
    }
    colnames(loadings.matrix) <- paste0("component", 1:k)
    table.df <- data.frame(loadings.matrix, check.names = FALSE)
    formatters <- list()
    for (i in 1:k)
        formatters[[paste0("component", i)]] <- createBarFormatter(decimals = 3,
                                                                   bar.shows.magnitude = TRUE,
                                                                   min.display.value = min.display.value)
    createTable(table.df, column.labels, formatters, title, subtitle, footer,
                no.wrap.column.headers = TRUE)
}
