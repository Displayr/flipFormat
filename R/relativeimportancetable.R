#' @title RelativeImportanceTable
#'
#' @description Creates a pretty formattable table for relative importance output.
#' @param relative.importance The relative importance object.
#' @param row.labels The row labels corresponding to the predictor variable names/labels.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @param p.cutoff The alpha level used when formatting the p-value column.
#' @references This is based on code written by Kenton Russell.
#' @export
RelativeImportanceTable <- function(relative.importance,
                                    row.labels,
                                    title = "",
                                    subtitle = "",
                                    footer = "",
                                    p.cutoff = 0.05)
{
    ria <- relative.importance
    coef.df <- data.frame(ria$importance, ria$raw.importance, ria$standard.errors,
                          ria$statistics, ria$p.values, check.names = FALSE)
    names(coef.df) <- c("importance", "raw.importance", "std.err", "t", "p")
    row.names(coef.df) <- row.labels

    formatters <- list(
        importance = createBarFormatter(bar.shows.magnitude = TRUE),
        raw.importance = x ~ FormatAsReal(x, decimals = 3),
        std.err = x ~ FormatAsReal(x, decimals = 3),
        t = x ~ FormatAsReal(x, decimals = 2),
        p = createPFormatter(p.cutoff)
    )
    column.names <- c("Relative importance", "Raw score", "Standard<br>Error",
                      paste0("<span style='font-style:italic;'>", ria$statistic.name, "</span>"),
                      "<span style='font-style:italic;'>p</span>")
    createTable(coef.df, column.names, formatters, title, subtitle, footer)
}
