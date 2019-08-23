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
#' @param title
#' @param footer Footer to show containing sample information.
#' @return An \code{htmlwidget} containing tables showing the output from an
#'   automatic text categorization.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
AutomaticCategorizationWidget <- function(categorization, sizes, base.size,
                                          examples, text.raw.by.categorization,
                                          title, footer)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("table.css", cata)
    addCss("details.css", cata)
    addCss("automaticcategorization.css", cata)

    cata("<div class=\"main-container\">")

    cata("<h1>", htmlText(title), "</h1>")

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
    cata(kable(t, align = c("r", "l", "l", "l"),
               format = "html", escape = FALSE,
               table.attr = "class=\"auto-categorization-table\""))

    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details category-summary\">",
         "Raw text", "</summary>")

    for (i in seq(categories))
    {
        cata("<details class=\"details raw-text-details\">")
        cata("<summary class=\"summary sub-details raw-text-summary\">",
             htmlText(categories[i]), "</summary>")

        row.numbers <- which(categorization == levels(categorization)[i])
        raw.text.matrix <- cbind(row.numbers, htmlText(text.raw.by.categorization[[i]]))
        colnames(raw.text.matrix) <- c("Case", "Text")
        cata(kable(raw.text.matrix, align = c("c", "l"),
                   format = "html", escape = FALSE,
                   table.attr = "class=\"auto-categorization-table\""))
        cata("</details>")
    }
    cata("</details>")


    # categories <- names(sizes)
    # for (i in seq(categories))
    # {
    #     cata("<details class=\"details\">")
    #     cata("<summary class=\"summary sub-details category-summary\">",
    #          paste0("Category ", i, " (",
    #          , "): ",
    #          "<span class=\"category-name\">", htmlText(categories[i]), "</span>"),
    #          "</summary>")
    #
    #     row.numbers <- which(categorization == levels(categorization)[i])
    #     raw.text.matrix <- cbind(row.numbers, htmlText(text.raw.by.categorization[[i]]))
    #
    #     categoryInfo(sizes[i], examples[i], raw.text.matrix, cata)
    #
    #     categoryRawText(raw.text.matrix, cata)
    #     cata("</details>")
    # }

    missing.text <- text.raw.by.categorization$`NA`
    if (!is.null(missing.text))
    {
        cata("<details class=\"details\">")
        cata("<summary class=\"summary sub-details category-summary\">",
             "<span>Missing cases</span>", "</summary>")
        t <- matrix(missing.text)
        colnames(t) <- "Text"
        cata(kable(t, align = c("l"),
                   format = "html", escape = FALSE,
                   table.attr = "class=\"auto-categorization-table\""))
        cata("</details>")
    }

    cata("<div class=\"footer\">", htmlText(footer), "</div>")

    cata("</div>")

    createWidgetFromFile(tfile)
}

# categoryInfo <- function(category.size, category.example, raw.text.matrix,
#                          cata)
# {
#     cata("<p class=\"category-info\"><span class=\"category-info-label\">Category size: </span>",
#          FormatWithDecimals(category.size, 1), "</p>")
#
#     cata("<p class=\"category-info\"><span class=\"category-info-label\">Example: </span>",
#          htmlText(category.example), "</p>")
# }

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
