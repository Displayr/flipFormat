#' MultinomialLogitTable
#'
#' Creates a pretty formattable table for multinomial logit output.
#' @param coefficients A matrix of coefficients from the regression.
#'  Its row and column names are used in the output table.
#' @param standard.errors A matrix of standard errors of the coefficients.
#'  Used to determine coefficient significance.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @param p.cutoff The alpha level used when determining significance.
#' @references This is based on code written by Kenton Russell.
#' @importFrom stats pnorm
#' @export
MultinomialLogitTable <- function(coefficients,
                                  standard.errors,
                                  title = "",
                                  subtitle = "",
                                  footer = "",
                                  p.cutoff = 0.05)
{
    coefs <- t(coefficients)
    zs <- coefs / t(standard.errors)
    ps <- 2 * (1 - pnorm(abs(zs)))
    k <- ncol(coefs)
    column.labels <- colnames(coefs)
    colnames(coefs) <- paste0("outcome", 1:k)
    colnames(zs) <- paste0("z", 1:k)
    colnames(ps) <- paste0("p", 1:k)
    coef.df <- data.frame(coefs, zs, ps, check.names = FALSE)
    formatters <- list()
    for (i in 1:k)
        formatters[[paste0("outcome", i)]] <- createEstimateFormatter(paste0("z", i), paste0("p", i), p.cutoff)
    # Removing unwanted variables (i.e., the variables that contain the p-values and z statistics)
    columns.to.exclude <- as.list(structure(rep(FALSE, 2 * k), names = c(colnames(zs), colnames(ps))))
    formatters <- c(formatters, columns.to.exclude)
    createTable(coef.df, column.labels, formatters, title, subtitle, footer)
}
