#' HistTable
#'
#' Creates a pretty formattable table showing histograms
#' @param data.values A dataframe, with each column containing the values to create a histogram.
#' @param title The title of the table.
#' @param subtitle The subtitle of the table.
#' @param footer The footer of the table.
#' @param bin.size Size of the bins used in the histogram. The breakpoints are given by \code{seq(bin.min, bin.max, bin.size)}.
#' @param bin.min Any value in \code{data.values} smaller then this will be truncated to \code{bin.min}.
#' @param bin.min Any value in \code{data.values} larger then this will be truncated to \code{bin.max}.
#' @param hist.width Width of the histogram cell in any valid CSS size unit
#' @param hist.height Height of the histogram cell
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
                      bin.min = 0,
                      bin.max = 100,
                      bin.size = 5,
                      hist.width = 60,
                      hist.height = 20,
                      ...)
{
    # Input needs to be a data.frame, because we use lapply
    if (!is.data.frame(data.values))
        data.values <- as.data.frame(data.values)

    histString <- function(xx) {
        xx[xx > bin.max] <- bin.max
        xx[xx < bin.min] <- bin.min
        counts <- round(hist(xx, plot=F, breaks=seq(bin.min,bin.max,bin.size))$counts/length(xx)*100, 1)
        as.character(as.tags(sparkline(counts, type="bar", zeroColor="lightgray", width=hist.width, height=hist.height)))}

    df <- data.frame(..., # extra stats to report
                     'Distribution'=unlist(lapply(data.values, histString)),
                     stringsAsFactors = FALSE, check.names = FALSE)

    ft <- createTable(df, colnames(df), list(), title, subtitle, footer)
    ft$dependencies <- c(ft$dependencies, getDependency("sparkline","sparkline"))
    ft
}


