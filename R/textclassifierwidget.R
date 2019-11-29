#' @title Display text classifier output as an \code{htmlwidget}
#'
#' @description Creates a \code{htmlwidget} summary of information for a
#' text classifier output from flipTextAnalysis.
#' @param observed.sizes The sizes of the observed categories.
#' @param base.size The weighted sample size.
#' @param examples Examples for each category.
#' @param predicted.counts Numeric vector of counts of the predicted categories
#' @param category.accuracy Numeric vector of accuracy over training sample.
#' @param overall.metrics Numeric vector of the overall performance metrics on the trained model.
#' @param cv.metrics Numeric matrix of the performance metrics on the cross validation data.
#' @param text.raw.by.categorization A list containing the raw text for each
#'   category.
#' @param missing Logical vector indicating which cases are missing.
#' @param title The title to show at the top.
#' @param footer Footer to show containing sample information.
#' @return An \code{htmlwidget} containing tables showing the output from an
#'   automatic text categorization.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @importFrom knitr kable
#' @export
TextClassifierWidget <- function(observed.sizes,
                                 base.size,
                                 examples,
                                 predicted.counts,
                                 category.accuracy,
                                 overall.metrics,
                                 cv.metrics,
                                 text.raw.by.categorization,
                                 missing,
                                 title,
                                 footer)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)
    addCss("textclassifier.css", cata)

    cata("<div class=\"main-container\">")

    cata("<h1>", htmlText(title), "</h1>")

    textClassifierSummaryTable(observed.sizes, base.size, examples, predicted.counts, category.accuracy,
                               overall.metrics, cv.metrics, text.raw.by.categorization, missing, cata)

    overall.statement <- metricPrint(overall.metrics, sample.type = "Training sample")
    footer <- paste0(footer, overall.statement, ".")
    if (!is.null(cv.metrics))
    {
        if(nrow(cv.metrics) == 1)
        {
            model.size <- as.numeric(row.names(cv.metrics))
            validation.size <- sum(observed.sizes) - model.size
            validation.statement <- metricPrint(cv.metrics[1, ],
                                                sample.type = paste0("Single Validation sample (model n = ", model.size,
                                                                     " and validation n = ", validation.size, ")"))
            footer <- paste0(footer, validation.statement, ".")
            cata("<div class=\"footer\">", htmlText(footer), "</div>")
        } else
        {
            model.sizes <- as.numeric(row.names(cv.metrics))
            validation.sizes <- sum(observed.sizes) - model.sizes
            metric.names <- colnames(cv.metrics)
            cv.metrics <- cbind(model.sizes, validation.sizes,
                                FormatAsPercent(cv.metrics[, 1]), round(cv.metrics[, 2:3], 3))
            colnames(cv.metrics) <- c("Model size", "Validation size", metric.names)
            rownames(cv.metrics) <- NULL
            table.explanation <- "\nTable below shows out of sample performance on different model and validation set sizes."
            footer <- paste0(footer, table.explanation)
            cata("<div class=\"footer\">", htmlText(footer), "</div>")
            cata("<div class=\"classifier-validation-table\">", kable(cv.metrics, format = "html",
                                                                      align = c("ccccc")),"</div>")
        }
    } else
        cata("<div class=\"footer\">", htmlText(footer), "</div>")
    cata("</div>")

    createWidgetFromFile(tfile)
}

metricPrint <- function(metrics, sample.type)
{
    paste0("\n", sample.type, " performance - accuracy: ", FormatAsPercent(metrics[1]),
           "; Cohen's kappa: ", FormatAsReal(metrics[2]),"; F1:", FormatAsReal(metrics[3]))
}


textClassifierSummaryTable <- function(observed.counts, base.size, examples, predicted.counts, category.accuracy,
                                       overall.metrics, cv.metrics, text.raw.by.categorization, missing, cata)
{
    max.rows <- 3000

    observed.n <- FormatWithDecimals(observed.counts, decimal.places = 0)
    predicted.n <- FormatWithDecimals(predicted.counts, decimal.places = 0)
    category.n <- FormatWithDecimals(category.accuracy * observed.counts, decimal.places = 0)
    categories <- names(observed.counts)
    t <- matrix(NA, nrow = length(categories), ncol = 6)
    colnames(t) <- c("", "Category", "Observed (n)", "Predicted (n)", "Accuracy (n)", "Example")
    t[, 1] <- paste0(seq(categories), ".")

    overall.predicted <- sum(predicted.counts)
    for (i in seq(categories))
    {
        t[i, 2] <- htmlText(categories[i])
        t[i, 3] <- paste0(FormatAsPercent(observed.counts[i] / base.size), "&nbsp;(",
                          observed.n[i], ")")
        if (!is.na(examples[i]))
            t[i, 4] <- paste0("\"",  htmlText(examples[i]), "\"")
        else
            t[i, 4] <- ""
        t[i, 5] <- paste0(FormatAsPercent(predicted.counts[i] / overall.predicted), "&nbsp;(",
                          predicted.n[i], ")")
        t[i, 6] <- paste0(FormatAsPercent(category.accuracy[i]), "&nbsp;(",
                          category.n[i], ")")
    }

    missing.text <- text.raw.by.categorization$`NA`

    # Create table
    cata("<table class=\"auto-categorization-table\"><thead>",
         "<th></th><th>Category</th><th>Observed (n)</th><th>Predicted (n)</th><th>Accuracy (n)</n><th>Example</th>",
         "</thead><tbody>")
    for (i in seq(categories))
    {
        cata("<tr class=\"table-row\">")
        cata(paste0("<td>", t[i, 1], "</td>"))
        cata(paste0("<td>", t[i, 2], "</td>"))
        cata(paste0("<td>", t[i, 3], "</td>"))
        cata(paste0("<td>", t[i, 5], "</td>"))
        cata(paste0("<td>", t[i, 6], "</td>"))
        cata("<td>")

        if (!is.na(examples[i]))
        {
            cata("<details class=\"details raw-text-category-details\">")
            cata("<summary class=\"summary sub-details raw-text-category-summary\">",
                 "<span>", t[i, 4], "</span></summary>")

            text.raw.cat <- text.raw.by.categorization[[i]]
            row.numbers <- names(text.raw.cat)
            raw.text.matrix <- cbind(row.numbers, htmlText(unname(text.raw.cat)))

            raw.text.matrix <- truncateRawTextTable(raw.text.matrix, max.rows)

            colnames(raw.text.matrix) <- c("Case", "Raw text")
            cata(kable(raw.text.matrix, align = c("c", "l"),
                       format = "html", escape = FALSE,
                       table.attr = "class=\"raw-text-table\""))
        }
        cata("</td></tr>")
    }

    # Missing text
    missing.text <- text.raw.by.categorization$`NA`
    if (!is.null(missing.text))
    {
        cata("<tr class=\"raw-text-row\"><td></td><td>Missing cases</td><td></td><td><td></td><td>")

        cata("<details class=\"details raw-text-category-details\">")
        cata("<summary class=\"summary sub-details raw-text-category-summary\">",
             "<span>", missing.text[1], "</span></summary>")

        t <- cbind(which(missing), missing.text)
        t <- truncateRawTextTable(t, max.rows)

        colnames(t) <- c("Case", "Text")
        cata(kable(t, align = c("c", "l"),
                   format = "html", escape = FALSE,
                   table.attr = "class=\"raw-text-table\""))
        cata("</details>")
        cata("</td></tr>")
    }
    cata("</tbody></table>")

}

truncateRawTextTable <- function(t, max.rows)
{
    if (nrow(t) > max.rows)
    {
        n.original.rows <- nrow(t)
        n.omitted.rows <- n.original.rows - max.rows
        t <- t[1:max.rows, ]
        t <- rbind(t, c("", htmlText(paste0("<TABLE TRUNCATED. ",
                                            n.omitted.rows,
                                            " ROWS OMITTED>"))))
    }
    t
}

