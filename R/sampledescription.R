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
#'
#' @export
SampleDescription <- function(n.total, n.subset, n.estimation, subset.label, weighted = TRUE, weight.label = "", missing, imputation.label = NULL, m, variable.description = "")
{
    # Warning if there is less than 50% data.
    missing.data.proportion <- 1 - n.estimation / n.subset
    if (missing.data.proportion > 0.50)
        warning(paste(FormatAsPercent(missing.data.proportion), "of the data is missing and has been excluded from the analysis.",
                      "Consider either filters to ensure that the data that is missing is in-line with your expectations,",
                      "or, set 'Missing Data' to another option."))
    # Creating description.
    missing.data <- n.estimation < n.subset
    imputation <-  missing == "Imputation (replace missing values with estimates)" | missing == "Multiple imputation"
    description <- BaseDescription(paste0("n = ", n.estimation," cases used in estimation"),
                                   n.total, n.subset, n.estimation, subset.label, weighted, weight.label)
    if (variable.description != "")
        variable.description <- paste0(variable.description, " ")
    if (missing.data | imputation)
        description <- paste(description, switch(missing,
                                                 "Error if missing data" = "",
                                                 "Exclude cases with missing data" = "cases containing missing values have been excluded;",
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
#'
#' @export
BaseDescription <- function(description.of.n,
                            n.total, n.subset, n.estimation, subset.label, weighted = TRUE, weight.label = "")
{
    base <- if(n.estimation < n.subset) paste0(" of a total sample size of ", n.subset) else ""
    if (n.subset < n.total)
        base <- paste0(base, " (", as.character(subset.label), ")")
    base <- paste0(base, ";")
    paste0(description.of.n,
           base,
           ifelse(weighted,
                  paste0(" data has been weighted (", weight.label, ");"),
                  ""))
}

