#' RandomForestTable
#'
#' Creates a pretty formattable table for random forest.
#' @param importance A matrix of importance values from the regression.
#' @param z.statistics A vector or matrix of z statistics of the importance values.
#' @param p.values A vector or matrix of p values of the importance values.
#' @param sort.by.importance Sort the last column in descending order.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Text to place in the footer of the table.
#' @param p.cutoff The alpha level used when determining significance.
#' @references This is based on code written by Kenton Russell.
#' @export
RandomForestTable <- function(importance,
                              z.statistics,
                              p.values,
                              sort.by.importance = TRUE,
                              title = "",
                              subtitle = "",
                              footer = "",
                              p.cutoff = 0.05)
{
    numeric.outcome <- ncol(importance) == 2

    if (sort.by.importance)
    {
        ind <- sort(importance[, ncol(importance)], decreasing = TRUE, index.return = TRUE)$ix
        importance <- importance[ind, ]
        if (!numeric.outcome)
        {
            z.statistics <- z.statistics[ind, ]
            p.values <- p.values[ind, ]
        }
        else
        {
            z.statistics <- z.statistics[ind]
            p.values <- p.values[ind]
        }
    }

    if (!numeric.outcome) # Classification
    {
        k <- ncol(importance) - 1
        column.labels <- colnames(importance)
        column.labels[k + 1] <- paste0("Importance (", column.labels[k + 1], ")")
        colnames(importance) <- c(paste0("importance", 1:k), "gini")
        colnames(z.statistics) <- paste0("z", 1:k)
        colnames(p.values) <- paste0("p", 1:k)
        coef.df <- data.frame(importance, z.statistics, p.values, check.names = FALSE)
        formatters <- list()
        for (i in 1:(k - 1))
            formatters[[paste0("importance", i)]] <- createHeatmapFormatter(paste0("z", i), paste0("p", i), p.cutoff, decimals = 3)
        formatters[[paste0("importance", k)]] <- createBarFormatter(decimals = 3)
        formatters[["gini"]] <- createBarFormatter()
        # Removing unwanted variables (i.e., the variables that contain the p-values and z statistics)
        columns.to.exclude <- as.list(structure(rep(FALSE, 2 * k), names = c(colnames(z.statistics), colnames(p.values))))
        formatters <- c(formatters, columns.to.exclude)
    }
    else # Regression
    {
        column.labels <- colnames(importance)
        column.labels[2] <- paste0("Importance (", column.labels[2], ")")
        colnames(importance) <- c("importance", "mse")
        coef.df <- data.frame(importance, check.names = FALSE)
        formatters <- list(importance = createBarFormatter(decimals = 3),
                           mse = createBarFormatter())
    }
    createTable(coef.df, column.labels, formatters, title, subtitle, footer)
}
