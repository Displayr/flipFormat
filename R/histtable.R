#' HistTable
#'
#' Creates a pretty formattable table showing histograms
#' @param data.values A dataframe, with each column containing the values to create a histogram.
#' @param title The title of the table.
#' @param subtitle The subtitle of the table.
#' @param footer The footer of the table.
#' @param bin.size Size of the bins used in the histogram. The breakpoints are given by \code{seq(bin.min, bin.max, bin.size)}.
#' @param bin.min Any value in \code{data.values} smaller then this will be truncated to \code{bin.min}.
#' @param bin.max Any value in \code{data.values} larger then this will be truncated to \code{bin.max}.
#' @param hist.width Width of the histogram cell in any valid CSS size unit
#' @param hist.height Height of the histogram cell
#' @param show.tooltips Whether to display tooltips of the bar heights
#' @param color.negative Whether to show negative bars in coral.
#' @param histogram.column.name Name for the histogram column.
#' @param ... Additional columns to add to the table.
#' @importFrom graphics hist
#' @importFrom htmltools as.tags
#' @importFrom htmlwidgets getDependency
#' @importFrom sparkline sparkline
#' @examples
#' dat <- data.frame(A=rpois(500,5), B=rpois(500,50), C=rpois(500,20))
#' print(HistTable(dat, 'Mean Probability'=c(5,50,100)))
#' @export

HistTable <- function(data.values,
                      title = "",
                      subtitle = "",
                      footer = "",
                      bin.size = 5,
                      bin.min = 0,
                      bin.max = 100,
                      hist.width = 100,
                      hist.height = 20,
                      show.tooltips = TRUE,
                      color.negative = FALSE,
                      histogram.column.name = "Distribution",
                      ...)
{
    # Input needs to be a data.frame, because we use lapply
    if (!is.data.frame(data.values))
        data.values <- as.data.frame(data.values)

    histString <- function(xx)
    {
        xx[xx > bin.max] <- bin.max
        xx[xx < bin.min] <- bin.min
        breaks <- seq(bin.min, bin.max, bin.size)
        counts <- round(hist(xx, plot = F, breaks = breaks, right = FALSE)$counts / length(xx) * 100, 1)

        if (color.negative)
        {
            positive.breaks <- breaks >= 0
            positive.counts <- rep(0, length(counts))
            positive.counts[positive.breaks] <- counts[positive.breaks]
            negative.counts <- rep(0, length(counts))
            negative.counts[!positive.breaks] <- counts[!positive.breaks]
            values <- cbind(positive.counts, negative.counts)
            as.character(as.tags(sparkline(values, type = "bar", zeroColor = "lightgray",
                                           width = hist.width, height = hist.height,
                                           stackedBarColor = c(positiveColour(), negativeColour()),
                                           disableInteraction = !show.tooltips)))
        }
        else
        {
            as.character(as.tags(sparkline(counts, type = "bar", zeroColor = "lightgray",
                                           width = hist.width, height = hist.height,
                                           barColor = positiveColour(),
                                           disableInteraction = !show.tooltips)))
        }
    }

    df <- data.frame("temp" = unlist(lapply(data.values, histString)),
                     ..., # extra stats to report
                     stringsAsFactors = FALSE, check.names = FALSE)
    names(df)[1] <- histogram.column.name

    ft <- createTable(df, colnames(df), list(), title, subtitle, footer,
                      col.names.alignment = c("c", rep("r", length(df) - 1)))
    ft$dependencies <- c(ft$dependencies, getDependency("sparkline","sparkline"))
    ft
}


