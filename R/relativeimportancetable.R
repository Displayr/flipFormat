#' @title RelativeImportanceTable
#'
#' @description Creates a pretty formattable table for relative importance output.
#' @param relative.importance The relative importance scores.
#' @param raw.importance The raw importance scores.
#' @param standard.errors Standard errors of the raw importance scores.
#' @param t.statistics t-statistics for the raw importance scores.
#' @param p.values p-values for the raw importance scores.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @param p.cutoff The alpha level used when formatting the p-value column.
#' @references This is based on code written by Kenton Russell.
#' @export
RelativeImportanceTable <- function(relative.importance,
                                    raw.importance,
                                    standard.errors,
                                    t.statistics,
                                    p.values,
                                    title = "",
                                    subtitle = "",
                                    footer = "",
                                    p.cutoff = 0.05)
{
    coef.df <- data.frame(raw.importance, standard.errors, t.statistics, p.values, check.names = FALSE)
    colnames(coef.df) <- c("relative.importance", "raw.importance", "std.err", "t", "p")
    formatters <- list(
        relative.importance = createEstimateFormatter("t", "p", p.cutoff),
        raw.importance = x ~ FormatWithDecimals(x, 3),
        std.err = x ~ FormatWithDecimals(x, 2),
        t = createHeatmapFormatter("t", "p", p.cutoff),
        p = createPFormatter(p.cutoff)
    )
    column.names <- c("Relative importance", "Raw score", "Standard<br>Error",
                      "<span style='font-style:italic;'>t</span>",
                      "<span style='font-style:italic;'>p</span>")
    createTable(coef.df, column.names, formatters, title, subtitle, footer)
}
