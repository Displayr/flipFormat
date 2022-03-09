#' @title SignificanceTest
#'
#' @description Standardize information to be shown in significance test results.
#' @param obj Significance testing output object, e.g. object of class htest.
#' @param test.name Name of the test.
#' @param vars Variables used in test.
#' @param filter Filter variable.
#' @param weight Weight variable.
#' @param p.value.method Specifies how the p-value is computed.
#' @param show.labels Whether to show variable labels instead of names.
#' @param decimal.places The number of decimal places to show.
#' @param missing How missing data is to be treated in the analysis.
#'  Options are: \code{"Error if missing data"}, \code{"Exclude cases with missing data"} and
#'  \code{"Imputation (replace missing values with estimates)"}.
#' @param reg.name The name of the regression object on which the test is run.
#' @param reg.sample.description The sample description of the regression object on which the test is run.
#' @param resample Whether resampling is used whenever weights are applied.
#' @param group.levels The levels of the categorical group variable.
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
SignificanceTest <- function(obj, test.name, vars = NULL, filter = NULL, weight = NULL, p.value.method = "",
                             show.labels = TRUE, decimal.places = NULL,
                             missing = "Exclude cases with missing data",
                             reg.name = NULL, reg.sample.description = NULL, resample = FALSE,
                             group.levels = NULL)
{
    result <- list()
    result$test.name <- test.name
    result$null.hypothesis <- nullHypothesis(obj, test.name)
    result$additional.footer <- ""

    if (inherits(obj, "htest"))
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
        if (is.null(reg.sample.description))
        {
            result$variable.text <- variableText(vars, show.labels, group.levels = group.levels)
            result$sample.description <- sampleDescriptionFromVariables(vars, filter, weight, missing, resample)
        }
        else
        {
            if (any(nzchar(reg.name)))
                result$variable.text <- paste("Regression:", reg.name)
            result$sample.description <- reg.sample.description
        }
    }
    else if (test.name %in% c("Bartlett Test of Sphericity"))
    {
        result$statistic <- obj$chisq
        result$statistic.name <- "Chi-square"
        result$degrees.of.freedom <- obj$df
        result$p.value <- obj$p.value
        result$variable.text <- variableText(vars, show.labels, multiple = TRUE)
        result$sample.description <- sampleDescriptionFromVariables(vars, filter, weight, missing,
                                                                    n.estimation = obj$n.estimation,
                                                                    imputation.label = obj$imputation.label,
                                                                    multiple = TRUE)
    }
    else if (test.name %in% c("Chi-Square Test of Independence"))
    {
        result$statistic <- obj$statistic
        result$statistic.name <- "Chi-square"
        result$degrees.of.freedom <- obj$df
        result$p.value <- obj$p.value
        result$variable.text <- variableTextWithCategories(vars, show.labels, filter)
        result$sample.description <- sampleDescriptionFromVariables(vars, filter, weight, missing)
    }
    else if (test.name == "Little's MCAR Test")
    {
        result$statistic <- obj$chi.square
        result$statistic.name <- "Chi-square"
        result$degrees.of.freedom <- obj$df
        result$p.value <- obj$p.value
        result$variable.text <- variableText(vars, show.labels, multiple = TRUE)
        result$sample.description <- sampleDescriptionFromVariables(vars, filter, weight, missing, multiple = TRUE)
    }
    else if (test.name == "Test of Residual Heteroscedasticity (Breusch-Pagan)")
    {
        result$statistic <- obj$ChiSquare
        result$statistic.name <- "Chi-square"
        result$degrees.of.freedom <- obj$Df
        result$p.value <- obj$p
        result$variable.text <- if (any(nzchar(reg.name))) paste("Regression:", reg.name)
        result$sample.description <- reg.sample.description
    }
    else
        stop(paste("Test not identified:", test.name))

    result$p.cutoff <- 0.05

    result$decimal.places <- decimal.places

    class(result) <- c("SignificanceTest", "visualization-selector")
    return(result)
}

#' @title print.SignificanceTest
#' @description print method for SignificanceTest objects.
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
                              "Two-Sample Wilcoxon Rank Sum Test"))
    {
        if (obj$alternative == "two.sided")
            paste0("true location shift is equal to ", obj$null.value)
        else if (obj$alternative == "less")
            paste0("true location shift is greater than ", obj$null.value)
        else if (obj$alternative == "greater")
            paste0("true location shift is less than ", obj$null.value)
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
            paste0("true correlation is greater than ", obj$null.value)
        else if (obj$alternative == "greater")
            paste0("true correlation is less than ", obj$null.value)
        else
            stop(paste("Alternative not recogised:", obj$alternative))
    }
    else if (test.name == "Little's MCAR Test")
        "missing data is Missing Completely At Random (MCAR)"
    else if (test.name == "Test of Residual Heteroscedasticity (Breusch-Pagan)")
        "residuals are homoscedastic"
    else if (test.name == "Test of Residual Normality (Shapiro-Wilk)")
        "residuals are normally distributed"
    else if (test.name == "Test of Residual Serial Correlation (Durbin-Watson)")
        "residuals are not serially correlated"
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

variableText <- function(vars, show.labels, multiple = FALSE, group.levels = NULL)
{
    if (multiple)
    {
        if (show.labels)
        {
            var.labels <- Labels(vars)
            extract <- ExtractCommonPrefix(var.labels)
            if (is.na(extract$common.prefix))
                paste(var.labels, collapse = ", ")
            else
                paste0(extract$common.prefix, ": ", paste(extract$shortened.labels, collapse = ", "))
        }
        else
            paste(sapply(vars, function(x) {attr(x, "name")}), collapse = ", ")
    }
    else if (length(vars) == 1)
    {
        if (show.labels)
            Labels(vars[[1]])
        else
            attr(vars[[1]], "name")
    }
    else if (length(vars) == 2)
    {
        lvls <- if (!is.null(group.levels)) levelsText(group.levels) else ""
        if (show.labels)
            c(Labels(vars[[1]]), paste("by", Labels(vars[[2]]), lvls))
        else
            paste(attr(vars[[1]], "name"), "by", attr(vars[[2]], "name"), lvls)
    }
    else
        stop("Variable length not handled.")
}

variableTextWithCategories <- function(vars, show.labels, filter)
{
    if (is.null(filter) || (length(filter) == 1 && filter)) # no filter applied
        filter <- rep(TRUE, length(vars[[1]]))

    if (length(vars) == 2)
    {
        levels1 <- levelsText(levels(factor(vars[[1]][filter])))
        levels2 <- levelsText(levels(factor(vars[[2]][filter])))
        if (show.labels)
            c(paste(Labels(vars[[1]]), levels1), paste("by", Labels(vars[[2]]), levels2))
        else
            c(paste(attr(vars[[1]], "name"), levels1), paste("by", attr(vars[[2]], "name"), levels2))
    }
    else
        stop("Variable length not handled.")
}

levelsText <- function(lvls)
{
    t <- paste(lvls, collapse = ", ")
    if (nchar(t) > 300)
        t <- paste0(substr(t, 1, 300), "...")
    paste0("(", t, ")")
}

sampleDescriptionFromVariables <- function(vars, filter, weight, missing, resample = FALSE, multiple = FALSE,
                                           imputation.label = NULL, n.estimation = NULL)
{
    if (is.null(filter) || (length(filter) == 1 && filter)) # no filter applied
        filter <- rep(TRUE, length(vars[[1]]))

    var.lengths <- sapply(vars, length)
    if (min(var.lengths) != max(var.lengths) || var.lengths[1] != length(filter) ||
        (!is.null(weight) && var.lengths[1] != length(weight)))
        stop("Input variables are of different lengths.")

    weight.label <- if (is.null(weight)) "" else Labels(weight)

    if (multiple)
    {
        dat <- data.frame(vars)
        if (is.null(n.estimation))
            n.estimation <- if (missing == "Exclude cases with all missing data")
                sum(apply(is.na(dat), 1, sum) < ncol(dat) & filter)
            else
                stop(paste("Missing data case not handled:", missing))
        SampleDescription(length(vars[[1]]), sum(filter), n.estimation, Labels(filter),
                          weighted = !is.null(weight), weight.label = weight.label,
                          missing = missing, resample = resample, imputation.label = imputation.label)
    }
    else if (length(vars) == 1)
    {
        v <- vars[[1]]
        if (is.null(n.estimation))
            n.estimation <- if (missing == "Exclude cases with missing data")
                sum(!is.na(v) & filter)
            else
                stop(paste("Missing data case not handled:", missing))
        SampleDescription(length(v), sum(filter), n.estimation, Labels(filter),
                          weighted = !is.null(weight), weight.label = weight.label,
                          missing = missing, resample = resample, imputation.label = imputation.label)
    }
    else if (length(vars) == 2)
    {
        v1 <- vars[[1]]
        v2 <- vars[[2]]
        if (is.null(n.estimation))
            n.estimation <- if (missing == "Exclude cases with missing data")
                sum(!is.na(v1) & !is.na(v2) & filter)
            else
                stop(paste("Missing data case not handled:", missing))
        SampleDescription(length(v1), sum(filter), n.estimation, Labels(filter),
                          weighted = !is.null(weight), weight.label = weight.label,
                          missing = missing, resample = resample, imputation.label = imputation.label)
    }
}

significanceTestTable <- function(obj)
{
    title <- obj$test.name
    secondary.title <- if (is.na(obj$p.value))
        "p-value: NA"
    else
    {
        significance <- if (obj$p.value > obj$p.cutoff) "Not significant" else "Significant"
        p.value.text <- if (is.null(obj$decimal.places))
        {
            formatted.p.value <- FormatAsPValue(obj$p.value)
            if (grepl("<", formatted.p.value))
                paste0(" ", formatted.p.value)
            else
                paste0(" = ", formatted.p.value)
        }
        else
            paste(" = ", FormatAsReal(obj$p.value, decimals = obj$decimal.places))
        paste0(significance, ": p-value", p.value.text)
    }
    subtitle <- obj$variable.text
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
            x ~ FormatAsReal(x, decimals = 2)
        else
            x ~ FormatAsReal(x, decimals = obj$decimal.places)
    }

    # Statistic
    dat.list$statistic <- obj$statistic
    col.names <- c(col.names, obj$statistic.name)
    formatters$statistic <- if (is.null(obj$decimal.places))
        x ~ FormatAsReal(x, decimals = 2)
    else
        x ~ FormatAsReal(x, decimals = obj$decimal.places)

    # Degrees of freedom
    if (!is.null(obj$degrees.of.freedom))
    {
        if (length(obj$degrees.of.freedom) == 1)
        {
            dof <- obj$degrees.of.freedom
            dat.list$df <- dof
            default.decimal.places <- if (floor(dof) == dof) 0 else 2
            col.names <- c(col.names, "Degrees of freedom")
            formatters$df <- if (is.null(obj$decimal.places))
                x ~ FormatAsReal(x, decimals = default.decimal.places)
            else
                x ~ FormatAsReal(x, decimals = obj$decimal.places)
        }
        else if (length(obj$degrees.of.freedom) == 2)
        {
            # This should be refactored when it is figured out how to
            # assign a formatter with an arbitrary name without a warning.
            dof <- obj$degrees.of.freedom[1]
            dat.list$df1 <- dof
            default.decimal.places <- if (floor(dof) == dof) 0 else 2
            col.names <- c(col.names, obj$degrees.of.freedom.name[1])
            formatters$df1 <- if (is.null(obj$decimal.places))
                x ~ FormatAsReal(x, decimals = default.decimal.places)
            else
                x ~ FormatAsReal(x, decimals = obj$decimal.places)

            dof <- obj$degrees.of.freedom[2]
            dat.list$df2 <- dof
            default.decimal.places <- if (floor(dof) == dof) 0 else 2
            col.names <- c(col.names, obj$degrees.of.freedom.name[2])
            formatters$df2 <- if (is.null(obj$decimal.places))
                x ~ FormatAsReal(x, decimals = default.decimal.places)
            else
                x ~ FormatAsReal(x, decimals = obj$decimal.places)
        }
        else
            stop("Degrees of freedom not handled!")
    }

    # p-value
    dat.list$p.value <- obj$p.value
    col.names <- c(col.names, "p-value")
    formatters$p.value <- if (is.null(obj$decimal.places))
        x ~ FormatAsPValue(x)
    else
        x ~ FormatAsReal(x, decimals = obj$decimal.places)

    # Confidence interval
    if (!is.null(obj$confidence.interval))
    {
        dat.list$lower.bound <- obj$confidence.interval[1]
        dat.list$upper.bound <- obj$confidence.interval[2]
        col.names <- c(col.names, "Lower 95%<br>conf. int.", "Upper 95%<br>conf. int.")
        formatters$lower.bound <- if (is.null(obj$decimal.places))
            x ~ FormatAsReal(x, decimals = 2)
        else
            x ~ FormatAsReal(x, decimals = obj$decimal.places)
        formatters$upper.bound <- if (is.null(obj$decimal.places))
            x ~ FormatAsReal(x, decimals = 2)
        else
            x ~ FormatAsReal(x, decimals = obj$decimal.places)
    }

    createTable(data.frame(dat.list), col.names, formatters, title, subtitle, footer,
                secondary.title = secondary.title)
}
