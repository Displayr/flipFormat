#' @title{SignificanceTest}
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
#' @param missing How missing data is to be treated in the analysis.
#'  Options are: \code{"Error if missing data"}, \code{"Exclude cases with missing data"} and
#'  \code{"Imputation (replace missing values with estimates)"}.
#' @details This function was created for the following Standard R pages:
#' \itemize{
#' \item{Missing Data - Little's MCAR Test}
#' \item{Regression - Diagnostic - Heteroscedasticity}
#' \item{Regression - Diagnostic - Normality (Shapiro-Wilk)}
#' \item{Regression - Diagnostic - Serial Correlation (Durbin-Watson)}
#' \item{Test - Bartlett Test of Sphericity}
#' \item{Test - Chi-Square Test of Independence}
#' \item{Test - Correlation}
#' \item{Test - Nonparametric - Kruskal-Wallis Rank Sum Test}
#' \item{Test - Nonparametric - Paired Samples Wilcoxon Test}
#' \item{Test - Nonparametric - Single-Sample Wilcoxon Test}
#' \item{Test - Nonparametric - Two-Sample Wilcoxon Rank Sum Test}
#' \item{Test - Variance - F-Test to Compare Two Variances}
#' }
#' @export
SignificanceTest <- function(obj, test.name, var1, var2 = NULL, filter, weight, p.value.method = "",
                             show.labels = TRUE, decimal.places = NULL,
                             missing = "Exclude cases with missing data")
{
    result <- list()
    result$test.name <- test.name
    result$null.hypothesis <- nullHypothesis(obj, test.name)
    result$additional.footer <- ""
    if (class(obj) == "htest")
    {
        result$estimate <- unname(obj$estimate)
        result$estimate.name <- names(obj$estimate)
        result$confidence.interval <- obj$conf.int
        result$statistic <- unname(obj$statistic)
        result$statistic.name <- names(obj$statistic)
        result$degrees.of.freedom <- unname(obj$parameter)
        result$degrees.of.freedom.name <- names(obj$parameter)
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
    else if (test.name == "Little's MCAR Test")
    {
        result$statistic <- obj$chi.square
        result$statistic.name <- "Chi-square"
        result$degress.of.freedom <- obj$df
        result$p.value <- obj$p.value
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
                                                   missing = missing)

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
        "ratio of variances is equal to 1"
    else if (test.name == "Bartlett Test of Sphericity")
        "true variances are equal"
    else if (test.name == "Chi-Square Test of Independence")
        "variables are independent"
    else if (test.name %in% c("Pearson's product-moment correlation",
                              "Spearman's rank correlation rho",
                              "Kendall's rank correlation tau"))
    {
        if (obj$alternative == "two.sided")
            paste0("true correlation is equal to ", obj$null.value)
        else if (obj$alternative == "less")
            paste0("true correlation is greater than or equal to ", obj$null.value)
        else if (obj$alternative == "greater")
            paste0("true correlation is less than or equal to ", obj$null.value)
        else
            stop(paste("Alternative not recogised:", obj$alternative))
    }
    else
        stop(paste("Test name not recognised:", test.name))
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

    dat.list <- list()
    col.names <- c()
    formatters <- list()

    # Estimate
    if (!is.null(obj$estimate))
    {
        dat.list$estimate <- obj$estimate
        col.names <- c(col.names, obj$estimate.name)
        formatters$estimate <- if (is.null(obj$decimal.places))
            x ~ FormatWithDecimals(x, 2)
        else
            x ~ FormatWithDecimals(x, obj$decimal.places)
    }

    # Statistic
    dat.list$statistic <- obj$statistic
    col.names <- c(col.names, obj$statistic.name)
    formatters$statistic <- if (is.null(obj$decimal.places))
        x ~ FormatWithDecimals(x, 2)
    else
        x ~ FormatWithDecimals(x, obj$decimal.places)

    # Degrees of freedom
    if (!is.null(obj$degrees.of.freedom))
    {
        if (length(obj$degrees.of.freedom) > 1)
        {
            for (i in 1:length(obj$degrees.of.freedom))
            {
                dat.list[paste0("df", i)] <- obj$degrees.of.freedom[i]
                col.names <- c(col.names, obj$degrees.of.freedom.name[i])
                formatters[paste0("df", i)] <- if (is.null(obj$decimal.places))
                    x ~ FormatWithDecimals(x, 0)
                else
                    x ~ FormatWithDecimals(x, obj$decimal.places)
            }
        }
        else
        {
            dat.list$df <- obj$degrees.of.freedom
            col.names <- c(col.names, "Degrees of freedom")
            formatters$df <- if (is.null(obj$decimal.places))
                x ~ FormatWithDecimals(x, 0)
            else
                x ~ FormatWithDecimals(x, obj$decimal.places)
        }
    }

    # p-value
    dat.list$p.value <- obj$p.value
    col.names <- c(col.names, "p-value")
    formatters$p.value <- if (is.null(obj$decimal.places))
        x ~ FormatAsPValue(x)
    else
        x ~ FormatWithDecimals(x, obj$decimal.places)

    # Confidence interval
    if (!is.null(obj$confidence.interval))
    {
        dat.list$lower.bound <- obj$confidence.interval[1]
        dat.list$upper.bound <- obj$confidence.interval[2]
        col.names <- c(col.names, "Lower 95% conf. int.", "Upper 95% conf. int.")
        formatters$lower.bound <- if (is.null(obj$decimal.places))
            x ~ FormatWithDecimals(x, 2)
        else
            x ~ FormatWithDecimals(x, obj$decimal.places)
        formatters$upper.bound <- if (is.null(obj$decimal.places))
            x ~ FormatWithDecimals(x, 2)
        else
            x ~ FormatWithDecimals(x, obj$decimal.places)
    }

    createTable(data.frame(dat.list), col.names, formatters, title, subtitle, footer)
}
