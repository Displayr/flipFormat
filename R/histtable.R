#' HistTable
#'
#' Creates a pretty formattable table showing histograms
#' @param data.values A dataframe, with each column containing the values to create a histogram.
#' @param title The title of the table.
#' @param subtitle The subtitle of the table.
#' @param footer The footer of the table.
#' @param ... Additional columns to add to the table.
#' @importFrom graphics hist
#' @importFrom htmltools as.tags
#' @importFrom htmlwidgets getDependency
#' @importFrom sparkline sparkline
#' @examples
#' dat <- data.frame(A=rpois(500,5), B=rpois(500,50), C=rpois(500,100))
#' print(HistTable(dat, Mean=c(5,50,100)))
#' @export

HistTable <- function(data.values,
                      title = "",
                      subtitle = "",
                      footer = "",
                      ...)
{
    # Input needs to be a data.frame, because we use lapply
    if (!is.data.frame(data.values))
        data.values <- as.data.frame(data.values)

    histString <- function(xx) {as.character(as.tags(sparkline(hist(xx,plot=F)$counts/length(xx)*100,
                                            type="bar", zeroColor="lightgray", chartRangeMin=0, chartRangeMax=100)))}
    df <- data.frame(..., # extra stats to report
                     Distribution=unlist(lapply(data.values, histString)),
                     stringsAsFactors = FALSE)

    ft <- createTable(df, colnames(df), list(), title, subtitle, footer)
    ft$dependencies <- c(ft$dependencies, getDependency("sparkline","sparkline"))
    ft
}


