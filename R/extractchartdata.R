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

#' @export
ExtractChartData.default <- function(x)
{
    # Outputs from CChart with append.data = TRUE can return data
    if (!is.null(attr(x, "ChartData")))
       return(attr(x, "ChartData"))
    else
        return(x)
}
