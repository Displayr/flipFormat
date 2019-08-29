#' Display automatic text categorization output as an \code{htmlwidget}
#'
#' Creates a \code{htmlwidget} summary of information for a
#' automatic text categorization output from flipTextAnalysis.
#' @param categorization A factor containing the categorization of the text.
#' @param sizes The sizes of the categories.
#' @param base.size The weighted sample size.
#' @param examples Examples for each category.
#' @param text.raw.by.categorization A list containing the raw text for each
#'   category.
#' @param missing Logical vector indicating which cases are missing.
#' @param title The title to show at the top.
#' @param footer Footer to show containing sample information.
#' @return An \code{htmlwidget} containing tables showing the output from an
#'   automatic text categorization.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
AutomaticCategorizationWidget <- function(categorization, sizes, base.size,
                                          examples, text.raw.by.categorization,
                                          missing, title, footer)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("automaticcategorization.css", cata)

    cata("<div class=\"main-container\">")

    cata("<h1>", htmlText(title), "</h1>")

        autoCategorizationSummaryTable(categorization, sizes, base.size,
                                       examples, text.raw.by.categorization,
                                       missing, cata)

    cata("<div class=\"footer\">", htmlText(footer), "</div>")

    cata("</div>")

    createWidgetFromFile(tfile)
}

autoCategorizationSummaryTable <- function(categorization, sizes, base.size,
                                           examples, text.raw.by.categorization,
                                           missing, cata)
{
    max.rows <- 3000

    n.text <- if (all(round(sizes) == sizes))
        FormatWithDecimals(sizes, decimal.places = 0)
    else
        FormatWithDecimals(sizes, decimal.places = 1)
    categories <- names(sizes)
    t <- matrix(NA, nrow = length(categories), ncol = 4)
    colnames(t) <- c("", "Category", "Size (n)", "Example")
    t[, 1] <- paste0(seq(categories), ".")
    for (i in seq(categories))
    {
        t[i, 2] <- htmlText(categories[i])
        t[i, 3] <- paste0(FormatAsPercent(sizes[i] / base.size), "&nbsp;(",
                          n.text[i], ")")
        t[i, 4] <- paste0("\"",  htmlText(examples[i]), "\"")
    }

    missing.text <- text.raw.by.categorization$`NA`

    # Create table
    cata("<table class=\"auto-categorization-table\"><thead>",
         "<th></th><th>Category</th><th>Size (n)</th><th>Example</th>",
         "</thead><tbody>")
    for (i in seq(categories))
    {
        cata("<tr class=\"table-row\">")
        cata(paste0("<td>", t[i, 1], "</td>"))
        cata(paste0("<td>", t[i, 2], "</td>"))
        cata(paste0("<td>", t[i, 3], "</td>"))

        cata("<td>")
        cata("<details class=\"details raw-text-category-details\">")
        cata("<summary class=\"summary sub-details raw-text-category-summary\">",
             "<span class=\"example-text\">",
             t[i, 4], "</span></summary>")

        row.numbers <- which(categorization == levels(categorization)[i])
        raw.text.matrix <- cbind(row.numbers, htmlText(text.raw.by.categorization[[i]]))

        raw.text.matrix <- truncateRawTextTable(raw.text.matrix, max.rows)

        colnames(raw.text.matrix) <- c("Case", "Raw text")
        cata(kable(raw.text.matrix, align = c("c", "l"),
                   format = "html", escape = FALSE,
                   table.attr = "class=\"raw-text-table\""))

        cata("</td></tr>")
    }

    # Missing text
    missing.text <- text.raw.by.categorization$`NA`
    if (!is.null(missing.text))
    {
        cata("<tr class=\"raw-text-row\"><td></td><td>Missing cases</td><td></td><td>")

        cata("<details class=\"details raw-text-category-details\">")
        cata("<summary class=\"summary sub-details raw-text-category-summary\">",
             "<div class=\"example-text\">", missing.text[1], "</div></summary>")

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

