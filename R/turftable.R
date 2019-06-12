#' TURFTable
#'
#' Creates a pretty formattable table for TURF output.
#' @param output.table A matrix of the statistics to be shown in the table.
#' @param portfolios A matrix containing the alternative indices of the TURF
#'  portfolios.
#' @param alternative.names The alternative names.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @export
TURFTable <- function(output.table,
                      portfolios,
                      alternative.names,
                      title = "",
                      subtitle = "",
                      footer = "")
{
    output.df <- data.frame(reach = output.table[, 1],
                            frequency = output.table[, 2])
    rownames(output.df) <- coloredAlternativeNames(portfolios,
                                                   alternative.names)
    formatters <- list(
        reach = x ~ FormatAsReal(x, decimals = 2),
        frequency = x ~ FormatAsReal(x, decimals = 0))

    column.labels <- colnames(output.table)
    createTable(output.df, column.labels, formatters, title, subtitle, footer)
}

coloredAlternativeNames <- function(portfolios, alternative.names)
{
    n.portfolios <- nrow(portfolios)
    portfolio.size <- ncol(portfolios)
    result <- character(n.portfolios)
    for (i in seq_len(n.portfolios))
    {
        colored.names <- character(portfolio.size)
        for (j in seq_len(portfolio.size))
        {
            alt.ind <- portfolios[i, j]
            colored.names[j] <- paste0(openTagPlaceholder(), "font color='",
                                       getAlternativeNameColor(alt.ind),
                                       "'", closeTagPlaceholder(),
                                       alternative.names[alt.ind],
                                       openTagPlaceholder(), "/font",
                                       closeTagPlaceholder())
        }
        result[i] <- paste0(colored.names, collapse = ", ")
    }
    result
}

getAlternativeNameColor <- function(alternative.index)
{
    officialColors()[((alternative.index - 1) %% 10) + 1]
}
