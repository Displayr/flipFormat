#' @title Display entity extraction output as an \code{htmlwidget}
#'
#' @description Creates a \code{htmlwidget} summary of information for an
#' entity extraction output from flipTextAnalysis.
#' @param entity.percentages named numeric vector showing the percentage ocurrence of entity types
#'     in the named entity recognition detection.
#' @param entity.counts named numeric vector showing the raw counts of entity types in the named
#'     entity recognition detection.
#' @param variant.percentages named numeric vector showing the percentage ocurrence of each
#'     variant for each entity type in the named entity recognition detection.
#' @param variant.counts named numeric vector showing the raw counts of each variant for each
#'     entity type in the named entity recognition detection.
#' @param title The title to show at the top.
#' @param footer Footer to show containing sample information.
#' @return An \code{htmlwidget} containing tables showing the output from an entity extraction.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
EntityExtractionWidget <- function(entity.percentages, variant.percentages, entity.counts,
                                   variant.counts, title, footer)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("entityextraction.css", cata)

    cata("<div class=\"main-container\">")

    cata("<h1>", htmlText(title), "</h1>")

    cata("<table class=\"entity-extraction-table\"><thead>",
         "<th>Entity</th><th>% (n)</th>",
         "</thead><tbody>")

    entity.percentages <- sort(entity.percentages, decreasing = TRUE, index.return = TRUE)
    entity.counts <- entity.counts[entity.percentages$ix]
    entity.percentages <- entity.percentages$x
    n.entities <- length(entity.percentages)

    for (i in 1:n.entities)
    {
        entity.name <- names(entity.percentages)[i]

        cata("<tr class=\"table-row\"><td>")

        cata("<details class=\"details entity-details\">")
        cata("<summary class=\"summary entity-summary\">",
             "<span>", htmlText(entity.name), "</span></summary>")

        percentages <- sort(variant.percentages[[entity.name]], decreasing = TRUE, index.return = TRUE)
        counts <- variant.counts[[entity.name]][percentages$ix]

        t <- cbind(htmlText(names(percentages$x)), unname(paste0(FormatAsPercent(percentages$x), " (", counts, ")")))

        colnames(t) <- c("Variants", "% (n)")
        cata(kable(t, align = c("c", "l"),
                   format = "html", escape = FALSE,
                   table.attr = "class=\"entity-variants-table\""))

        cata("</details>")
        cata("</td><td>")

        cata(FormatAsPercent(entity.percentages[i]), " (", entity.counts[i], ")", sep = "")

        cata("</td></tr>")
    }

    cata("</tbody></table>")

    cata("<div class=\"footer\">", htmlText(footer), "</div>")

    cata("</div>")

    createWidgetFromFile(tfile)
}
