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
    alt.names <- coloredAlternativeNames(portfolios, alternative.names)

    output.df <- data.frame(portfolio = alt.names,
                            reach = output.table[, 1],
                            frequency = output.table[, 2])
    rownames(output.df) <- paste0(seq_len(nrow(output.table)), " ")

    reach.lower <- min(output.table[, 1])
    reach.upper <- max(output.table[, 1])

    formatters <- list(
        portfolio = x ~ x,
        reach = createSingleColourHeatmapFormatter(reach.lower, reach.upper,
                                                   decimals = 1),
        frequency = createBarFormatter(decimals = 0))

    column.labels <- c("Portfolio", colnames(output.table))
    createTable(x = output.df, col.names = column.labels,
                formatters = formatters, title = title, subtitle = subtitle,
                footer = footer, col.names.alignment = c("l", "r", "r"))
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
            colored.names[j] <- alternativeNameHtml(alternative.names, alt.ind)
        }
        result[i] <- paste0(colored.names, collapse = ", ")
    }
    result
}

alternativeNameHtml <- function(alternative.names, alt.ind)
{
    paste0(openTagPlaceholder(), "font color='",
           getAlternativeNameColor(alt.ind),
           "' style='font-weight: bold;'", closeTagPlaceholder(),
           alternative.names[alt.ind], openTagPlaceholder(), "/font",
           closeTagPlaceholder())
}

getAlternativeNameColor <- function(alternative.index)
{
    officialColors()[((alternative.index - 1) %% 10) + 1]
}
