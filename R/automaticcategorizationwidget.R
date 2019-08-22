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
#' @param footer Footer to show containing sample information.
#' @return An \code{htmlwidget} containing tables showing the output from an
#'   automatic text categorization.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
AutomaticCategorizationWidget <- function(categorization, sizes, base.size,
                                          examples, text.raw.by.categorization,
                                          footer)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("table.css", cata)
    addCss("details.css", cata)
    addCss("automaticcategorization.css", cata)

    cata("<div class=\"main-container\">")

    cata("<h1>Automatic Categorization</h1>")

    categories <- names(sizes)
    for (i in seq(categories))
    {
        cata("<details class=\"details\">")
        cata("<summary class=\"summary sub-details category-summary\">",
             paste0("Category ", i, " (",
             FormatAsPercent(sizes[i] / base.size), "): ",
             "<span class=\"category-name\">", htmlText(categories[i]), "</span>"),
             "</summary>")

        row.numbers <- which(categorization == levels(categorization)[i])
        raw.text.matrix <- cbind(row.numbers, htmlText(text.raw.by.categorization[[i]]))

        categoryInfo(sizes[i], examples[i], raw.text.matrix, cata)

        categoryRawText(raw.text.matrix, cata)
        cata("</details>")
    }

    cata("<div class=\"footer\">", footer, "</div>")

    cata("</div>")

    createWidgetFromFile(tfile)
}

categoryInfo <- function(category.size, category.example, raw.text.matrix,
                         cata)
{
    cata("<p class=\"category-info\"><span class=\"category-info-label\">Category size: </span>",
         FormatWithDecimals(category.size, 1), "</p>")

    cata("<p class=\"category-info\"><span class=\"category-info-label\">Example: </span>",
         htmlText(category.example), "</p>")
}

categoryRawText <- function(raw.text.matrix, cata)
{
    cata("<details class=\"details raw-text-details\">")
    cata("<summary class=\"summary sub-details raw-text-summary\">",
         paste0("Raw text (", nrow(raw.text.matrix), " cases)"), "</summary>")
    colnames(raw.text.matrix) <- c("Case", "Text")
    cata(kable(raw.text.matrix, align = c("c", "l"),
               format = "html", escape = FALSE,
               table.attr = "class=\"auto-categorization-table\""))
    cata("</details>")
}
