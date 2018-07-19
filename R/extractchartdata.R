#' \code{ExtractChartData}
#'
#' @description Returns data which is appropriate for charting
#'   The exact form of the data depends on the object
#' @param x Object containing the data. This is usually the output of a regression or machine learning output
#' @export
ExtractChartData <- function(x)
{
    UseMethod("ExtractChartData")
}
