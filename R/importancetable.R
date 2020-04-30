#' @title Create formattable table of importance scores
#'
#' @description Creates a pretty formattable table for relative/Shapley importance output.
#' @param driver.analysis.output A list containing the relative importance scores, output coefficients
#'   and significance test values. The list needs to contain the following elements:
#'   \itemize{
#'   \item \code{importance}: A numeric vector of relative importance values based off the \code{raw.importance.scores}
#'   \item \code{raw.importance.scores}: A numeric vector of coefficients, these could be the output coefficients from a Shapley
#'    Regression or Relative Importance Analysis or the Jaccard Coefficients or Correlation coefficients if the \code{output.type} (see below) is
#'    \code{"Shapley Regression"}, \code{"Relative Importance Analysis"}, \code{"Jaccard Coefficient"} or \code{"Correlation"}
#'    respectively.
#'     \item \code{p.values}: The resulting p values from the statistics (see below).
#'    }
#'
#'    For \code{"Shapley Regression"} and \code{"Relative Importance Analysis"}, two more list elements are required,
#'    \itemize{
#'    \item \code{standard.errors}: The standard errors of the estimated coefficients
#'    \item \code{statistics}: The computed statistics
#'    }
#'    For \code{"Jaccard Coefficient"} or \code{"Correlation"} outputs, the following single extra elements is required instead.
#'    \itemize{
#'     \item \code{sample.size} The sample size count used in the calculation of significance tests for each driver.
#'   }
#' @param row.labels The row labels corresponding to the predictor variable names/labels.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @param p.cutoff The alpha level used when formatting the p-value column.
#' @param output.type A character string denoting the output type
#'   \itemize{
#'     \item \code{"Relative Importance Analysis"}
#'     \item \code{"Shapley Regression"}
#'     \item \code{"Jaccard Coefficient"}
#'     \item \code{"Correlation"}
#'   }

#' @references This is based on code written by Kenton Russell.
#' @export
ImportanceTable <- function(driver.analysis.output,
                            row.labels,
                            title = "",
                            subtitle = "",
                            footer = "",
                            p.cutoff = 0.05,
                            output.type)
{
    # Distinguish different cases
    if (output.type %in% c("Relative Importance Analysis", "Shapley Regression"))
    {
        coef.df <- data.frame(driver.analysis.output$importance, driver.analysis.output$raw.importance,
                              driver.analysis.output$standard.errors, driver.analysis.output$statistics,
                              driver.analysis.output$p.values, check.names = FALSE)
        names(coef.df) <- c("importance", "raw.importance", "std.err", "t", "p")
        column.names <- c("Importance", "Raw score", "Standard<br>Error",
                          paste0("<span style='font-style:italic;'>", driver.analysis.output$statistic.name, "</span>"),
                          "<span style='font-style:italic;'>p</span>")
    } else
    { # The Jaccard or Correlation output case
        coef.df <- data.frame(driver.analysis.output$importance, driver.analysis.output$raw.importance,
                              driver.analysis.output$sample.size, driver.analysis.output$p.values, check.names = FALSE)
        names(coef.df) <- c("importance", "raw.importance", "sample.size", "p")
        column.names <- c("Importance", output.type, "Sample<br>Size", "<span style='font-style:italic;'>p</span>")
    }

    row.names(coef.df) <- row.labels

    formatters <- list(
        importance = createBarFormatter(bar.shows.magnitude = TRUE),
        raw.importance = x ~ FormatAsReal(x, decimals = 3),
        std.err = x ~ FormatAsReal(x, decimals = 3),
        sample.size = x ~ FormatAsReal(x, decimals = 0),
        t = x ~ FormatAsReal(x, decimals = 2),
        p = createPFormatter(p.cutoff)
    )

    createTable(coef.df, column.names, formatters, title, subtitle, footer)
}
