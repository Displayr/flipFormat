#' \code{SignificanceTest}
#'
#' @description Standardize information to be shown in significance test results.
#' @param obj Significance testing output object, e.g. object of class htest.
#' @param test.name Name of the test.
#' @param var1 Variable 1 used in test.
#' @param var2 Variable 2 used in test.
#' @param filter Filter variable.
#' @param weight Weight variable.
#' @param p.value.method Specifies how the p-value is computed.
#' @param show.labels Whether to show variable labels instead of names.
#' @param decimal.places The number of decimal places to show.
#' @export
SignificanceTest <- function(obj, test.name, var1, var2 = NULL, filter, weight, p.value.method = "", show.labels = TRUE,
                             decimal.places = NULL)
{
    result <- list()
    result$test.name <- test.name
    result$null.hypothesis <- nullHypothesis(obj, test.name)
    result$additional.footer <- ""
    if (class(obj) == "htest")
    {
        result$statistic <- unname(obj$statistic)
        result$statistic.name <- names(obj$statistic)
        result$degrees.of.freedom <- unname(obj$parameter)
        result$p.value <- obj$p.value
        result$additional.footer <- pValueMethodText(p.value.method)
    }
    else if (test.name %in% c("Bartlett Test of Sphericity", "Chi-Square Test of Independence"))
    {
        result$statistic <- unname(obj$statistic)
        result$statistic.name <- names(obj$statistic)
        result$degrees.of.freedom <- unname(obj$df)
        result$p.value <- unname(obj$p.value)
    }
    else
        stop(paste("Test not identified:", test.name))

    result$show.labels <- show.labels

    result$var1.name <- attr(var1, "name")
    result$var2.name <- if (is.null(var2)) "" else attr(var2, "name")
    result$var1.label <- Labels(var1)
    result$var2.label <- if (is.null(var2)) "" else Labels(var2)

    if ((!is.null(var2) && length(var1) != length(var2)) ||
        length(var1) != length(filter) ||
        (!is.null(weight) && length(var1) != length(weight)))
        stop("Input variables are of different length.")
    weight.label <- if (is.null(weight)) "" else Labels(weight)
    n.estimation <- if (is.null(var2))
        sum(!is.na(var1) & filter)
    else
        sum(!is.na(var1) & !is.na(var2) & filter)

    result$sample.description <- SampleDescription(length(var1), sum(filter), n.estimation, Labels(filter),
                                                   weighted = !is.null(weight), weight.label = weight.label,
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

nullHypothesis <- function(obj, test.name)
{
    if (test.name == "Kruskal-Wallis Rank Sum Test")
        "mean ranks of the outcome are the same between groups"
    else if (test.name %in% c("Paired Samples Wilcoxon Test",
                              "Single-Sample Wilcoxon Test",
                              "Wilcoxon rank sum test"))
    {
        if (obj$alternative == "two.sided")
            paste0("true location shift is equal to ", obj$null.value)
        else if (obj$alternative == "less")
            paste0("true location shift is greater than or equal to ", obj$null.value)
        else if (obj$alternative == "greater")
            paste0("true location shift is less than or equal to ", obj$null.value)
        else
            stop(paste("Alternative not recogised:", obj$alternative))
    }
    else if (test.name == "F-Test to Compare Two Variances")
        "variances are equal"
    else if (test.name == "Bartlett Test of Sphericity")
        "variances are equal"
    else if (test.name == "Chi-Square Test of Independence")
        "variables are independent"
    else if (test.name == "Correlation")
    {
        if (obj$alternative == "two.sided")
            paste0("correlation is equal to ", obj$null.value)
        else if (obj$alternative == "less")
            paste0("correlation is greater than or equal to ", obj$null.value)
        else if (obj$alternative == "greater")
            paste0("correlation is less than or equal to ", obj$null.value)
        else
            stop(paste("Alternative not recogised:", obj$alternative))
    }
}

pValueMethodText <- function(p.value.method)
{
    if (p.value.method == "Exact")
        "exact p-value computed;"
    else if (p.value.method == "Normal approximation")
        "p-value computed using a normal approximation;"
    else if (p.value.method == "Normal approximation with continuity correction")
        "p-value computed using a normal approximation with continuity correction;"
    else
        ""
}

significanceTestTable <- function(obj)
{
    title <- if (obj$show.labels)
    {
        if (obj$var2.label == "")
            paste0(obj$test.name, ": ", obj$var1.label)
        else
            paste0(obj$test.name, ": ", obj$var1.label, " by ", obj$var2.label)
    }
    else
    {
        if (obj$var2.name == "")
            paste0(obj$test.name, ": ", obj$var1.name)
        else
            paste0(obj$test.name, ": ", obj$var1.name, " by ", obj$var2.name)
    }

    p.value.text <- if (is.null(obj$decimal.places))
    {
        formatted.p.value <- FormatAsPValue(obj$p.value)
        if (grepl(formatted.p.value, "<"))
            paste0(" ", formatted.p.value)
        else
            paste0(" = ", formatted.p.value)
    }
    else
        FormatWithDecimals(obj$p.value, obj$decimal.places)

    significance <- if (obj$p.value > obj$p.cutoff) "Not significant" else "Significant"
    subtitle <- paste0(significance, ": p-value", p.value.text)
    footer <- paste0(obj$sample.description, " null hypothesis: ", obj$null.hypothesis, ";")
    if (obj$additional.footer != "")
        footer <- paste(footer, obj$additional.footer)

    if (is.null(obj$degrees.of.freedom))
    {
        dat <- data.frame(statistic = obj$statistic,
                          p = obj$p.value)

        col.names <- c(obj$statistic.name, "p-value")

        formatters <- if (is.null(obj$decimal.places))
            list(statistic = x ~ FormatWithDecimals(x, 1),
                 p = x ~ FormatAsPValue(x))
        else
            list(statistic = x ~ FormatWithDecimals(x, obj$decimal.places),
                 p = x ~ FormatWithDecimals(x, obj$decimal.places))
    }
    else
    {
        dat <- data.frame(statistic = obj$statistic,
                          df = obj$degrees.of.freedom,
                          p = obj$p.value)

        col.names <- c(obj$statistic.name, "Degrees of freedom", "p-value")

        formatters <- if (is.null(obj$decimal.places))
            list(statistic = x ~ FormatWithDecimals(x, 1),
                 df = x ~ FormatWithDecimals(x, 1),
                 p = x ~ FormatAsPValue(x))
        else
            list(statistic = x ~ FormatWithDecimals(x, obj$decimal.places),
                 df = x ~ FormatWithDecimals(x, obj$decimal.places),
                 p = x ~ FormatWithDecimals(x, obj$decimal.places))
    }

    createTable(dat, col.names, formatters, title, subtitle, footer)
}
