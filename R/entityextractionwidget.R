#' @title Display entity extraction output as an \code{htmlwidget}
#'
#' @description Creates a \code{htmlwidget} summary of information for an
#' entity extraction output from flipTextAnalysis.
#' @param entity.percentages The percentage of cases for each entity.
#' @param variant.percentages The percentage for each variant.
#' @param title The title to show at the top.
#' @param footer Footer to show containing sample information.
#' @return An \code{htmlwidget} containing tables showing the output from an
#'   entity extraction.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
EntityExtractionWidget <- function(entity.percentages, variant.percentages,
                                   title, footer)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("entityextraction.css", cata)

    cata("<div class=\"main-container\">")

    cata("<h1>", htmlText(title), "</h1>")

    cata("<table class=\"entity-extraction-table\"><thead>",
         "<th>Entity</th><th>%</th>",
         "</thead><tbody>")

    entity.percentages <- sort(entity.percentages, decreasing = TRUE)

    n.entities <- length(entity.percentages)
    for (i in 1:n.entities)
    {
        entity.name <- names(entity.percentages)[i]

        cata("<tr class=\"table-row\"><td>")

        cata("<details class=\"details entity-details\">")
        cata("<summary class=\"summary entity-summary\">",
             "<span>", htmlText(entity.name), "</span></summary>")

        percentages <- sort(variant.percentages[[entity.name]], decreasing = TRUE)

        t <- cbind(htmlText(names(percentages)), unname(FormatAsPercent(percentages)))

        colnames(t) <- c("Variants", "%")
        cata(kable(t, align = c("c", "l"),
                   format = "html", escape = FALSE,
                   table.attr = "class=\"entity-variants-table\""))

        cata("</details>")
        cata("</td><td>")

        cata(FormatAsPercent(entity.percentages[i]))

        cata("</td></tr>")
    }

    cata("</tbody></table>")

    cata("<div class=\"footer\">", htmlText(footer), "</div>")

    cata("</div>")

    createWidgetFromFile(tfile)
}
