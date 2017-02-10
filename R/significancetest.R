#' \code{SignificanceTest}
#'
#' @description Standardize information to be shown in significance test results.
#' @param obj Significance testing output object, e.g. object of class htest.
#' @param var1 Variable 1 used in test.
#' @param var2 Variable 2 used in test.
#' @param filter Filter variable.
#' @param weight Weight variable.
#' @param show.labels Whether to show variable labels instead of names.
#' @param decimal.places The number of decimal places to show.
#' @export
SignificanceTest <- function(obj, var1, var2, filter, weight, show.labels = TRUE, decimal.places = NULL)
{
    result <- list()
    if (class(obj) == "htest")
    {
        result$statistic <- unname(obj$statistic)
        result$statistic.name <- names(obj$statistic)
        result$degrees.of.freedom <- unname(obj$parameter)
        result$p.value <- obj$p.value
        result$test.name <- obj$method
        result$null.hypothesis <- nullHypothesis(result$test.name)
    }
    else
    {

    }

    result$show.labels <- show.labels

    result$var1.name <- attr(var1, "name")
    result$var2.name <- attr(var2, "name")
    result$var1.label <- Labels(var1)
    result$var2.label <- Labels(var2)

    if (length(var1) != length(var2) ||
        length(var1) != length(filter) ||
        (!is.null(weight) && length(var1) != length(weight)))
        stop("Input variables are of different length.")
    weight.label <- if (is.null(weight)) "" else Labels(weight)

    result$sample.description <- SampleDescription(length(var1), sum(filter),
                                                   sum(!is.na(var1) & !is.na(var2) & filter), Labels(filter),
                                                   weighted = is.null(weight),
                                                   weight.label = weight.label,
                                                   missing = "Exclude cases with missing data")

    result$p.cutoff <- 0.05

    result$decimal.places <- decimal.places

    class(result) <- "SignificanceTest"
    return(result)
}

#' \code{print.SignificanceTest}
#' @param x A \link{SignificanceTest} object
#' @param ... Additional parameters to \code{\link{print.SignificanceTest}}
#' @export
print.SignificanceTest <- function(x, ...)
{
    print(significanceTestTable(x))
}

nullHypothesis <- function(test.name)
{
    "mean ranks of the groups are the same"
}

significanceTestTable <- function(obj)
{
    title <- if (obj$show.labels)
        paste0(obj$test.name, ": ", obj$var1.label, " by ", obj$var2.label)
    else
        paste0(obj$test.name, ": ", obj$var1.name, " by ", obj$var2.name)

    p.value.text <- if (is.null(obj$decimal.places))
        FormatAsPValue(obj$p.value)
    else
        FormatWithDecimals(obj$p.value, obj$decimal.places)

    significance <- if (obj$p.value > obj$p.cutoff) "Not significant" else "Significant"
    subtitle <- paste0(significance, ": p-value = ", p.value.text)
    # footer <- paste0("Null hypothesis: ", obj$null.hypothesis)
    footer <- obj$sample.description

    formatters <- if (is.null(obj$decimal.places))
        list(statistic = x ~ FormatWithDecimals(x, 1),
             df = x ~ FormatWithDecimals(x, 1),
             p = x ~ FormatAsPValue(x))
    else
        list(statistic = x ~ FormatWithDecimals(x, obj$decimal.places),
             df = x ~ FormatWithDecimals(x, obj$decimal.places),
             p = x ~ FormatWithDecimals(x, obj$decimal.places))

    dat <- data.frame(statistic = obj$statistic,
                      df = obj$degrees.of.freedom,
                      p = obj$p.value)
    col.names <- c(obj$statistic.name, "Degrees of freedom", "p-value")
    createTable(dat, col.names, formatters, title, subtitle, footer)
}
