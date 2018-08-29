#' \code{SampleDescription}
#'
#' @description Describes the sample, for use as footers in multivariate
#'   analyses.
#' @param n.total  Total number of observations in the database.
#' @param n.subset Total number of observations in the subset (less than or
#'   equal to \code{n.total}).
#' @param n.estimation The total number of observations used in estimation (less
#'   than or equal to \code{subset}).
#' @param subset.label E.g., "Males living in New York".
#' @param weighted Whether sample has been weighted.
#' @param weight.label The label of the weight.
#' @param missing How missing data is to be treated in the analysis. Options
#'   are: \code{"Error if missing data"}, \code{"Exclude cases with missing
#'   data"}, ,and \code{"Imputation (replace missing values with estimates)"}.
#' @param imputation.label Method used to impute the data.
#' @param m Number of imputaton samples.
#' @param variable.description A \code{char} specifying the type of
#' variables being imputed. Used in the description of the imputation
#' that typically appears in a footer.
#' @param resample Whether resampling is used whenever weights are applied.
#' @param effective.sample.size The effective sample size when weights are applied.
#' @return character description of sample
#' @export
SampleDescription <- function(n.total, n.subset, n.estimation, subset.label, weighted = TRUE,
                              weight.label = "", missing, imputation.label = NULL, m,
                              variable.description = "", resample = FALSE,
                              effective.sample.size = NULL)
{
    # Warning if there is less than 50% data.
    missing.data.proportion <- if (n.subset)
                                   1 - n.estimation / n.subset
                               else 1
    if (missing.data.proportion > 0.50)
        warning(paste(FormatAsPercent(missing.data.proportion), "of the data is missing and has been excluded from the analysis.",
                      "Consider either filters to ensure that the data that is missing is in-line with your expectations,",
                      "or, set 'Missing Data' to another option."))
    # Creating description.
    missing.data <- n.estimation < n.subset
    imputation <-  missing == "Imputation (replace missing values with estimates)" | missing == "Multiple imputation"
    description <- BaseDescription(paste0("n = ", n.estimation, " cases used in estimation"),
                                   n.total, n.subset, n.estimation,
                                   subset.label, weighted, weight.label,
                                   resample, effective.sample.size)
    if (variable.description != "")
        variable.description <- paste0(variable.description, " ")
    if (missing.data)
        description <- paste(description, switch(missing,
                                                 "Error if missing data" = "",
                                                 "Exclude cases with missing data" = "cases containing missing values have been excluded;",
                                                 "Exclude cases with all missing data" = "cases containing all missing values have been excluded;",
                                                 "Imputation (replace missing values with estimates)" =
                                                     paste0("missing values of ", variable.description, "variables have been imputed using ", imputation.label, ";"),
                                                 "Multiple imputation" =
                                                     paste0("multiple imputation (m = ", m, ", ", imputation.label, ") has been used to impute missing values of predictor variables;")))
    description
}



#' \code{BaseDescription}
#'
#' @description Describes the sample, for use as footers in multivariate
#'   analyses.
#' @param description.of.n A description of the sample (e.g., "People").
#' @param n.total  Total number of observations in the database.
#' @param n.subset Total number of observations in the subset (less than or
#'   equal to \code{n.total}).
#' @param n.estimation The total number of observations used in estimation (less
#'   than or equal to \code{subset}).
#' @param subset.label E.g., "Males living in New York".
#' @param weighted Whether sample has been weighted.
#' @param weight.label The label of the weight.
#' @param resample Whether resampling is used whenever weights are applied.
#' @param effective.sample.size The effective sample size when weights are applied.
#' @export
BaseDescription <- function(description.of.n, n.total, n.subset, n.estimation, subset.label,
                            weighted = TRUE, weight.label = "", resample = FALSE,
                            effective.sample.size = NULL)
{
    base <- if(n.estimation < n.subset) paste0(" of a total sample size of ", n.subset) else ""
    if (n.subset < n.total && sum(nchar(as.character(subset.label)), na.rm = TRUE) > 0)
        base <- paste0(base, " (", as.character(subset.label), ")")
    weight.text <- if (!is.null(weighted) && weighted)
    {
        if (resample)
            paste0(" data has been weighted via resampling (", weight.label, ");")
        else
            paste0(" data has been weighted (", weight.label, ");")
    }
    else
        ""
    ess.text <- if (!is.null(weighted) && weighted && !is.null(effective.sample.size))
        paste0(" effective sample size: ",
               FormatAsReal(effective.sample.size, decimals = 2), ";")
    else
        ""
    paste0(description.of.n, base, ";", weight.text, ess.text)
}

