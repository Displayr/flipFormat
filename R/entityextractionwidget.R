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

    if(all(entity.counts == 0)) {
        user.empty.msg <- paste0("No entities found to extract from dataset \n",
                                 "Use the 'Add named entities to extraction' control if you wish ",
                                 "to add entities to extract from the text.")
        cata("<tr class=\"table-row\"><td>")


        cata("<span>", htmlText(user.empty.msg), "</span>")
        cata("</td><td>")

        cata("0 (0)", sep = "")

        cata("</td></tr>")
        cata("</details>")
    } else
    {
        order.entity <- order(entity.counts, xtfrm(names(entity.counts)),
                              decreasing = c(TRUE, FALSE),
                              method = "radix")
        entity.percentages <- entity.percentages[order.entity]
        entity.counts <- entity.counts[order.entity]
        n.entities <- length(entity.counts)

        mapply(function(x, y, z, a, b) {
            cata("<tr class=\"table-row\"><td>")

            cata("<details class=\"details entity-details\">")
            cata("<summary class=\"summary entity-summary\">",
                "<span>", htmlText(z), "</span></summary>")
            variant.order <- order(x, xtfrm(names(x)),
                                   decreasing = c(TRUE, FALSE),
                                   method = "radix")
            x <- x[variant.order]
            y <- y[variant.order]
            t <- cbind(htmlText(names(y)),
                       unname(paste0(FormatAsPercent(y), " (", x, ")")))
            colnames(t) <- c("Variants", "% (n)")
            cata(kable(t, align = c("l", "l"),
                      format = "html", escape = FALSE,
                      table.attr = "class=\"entity-variants-table\""))
            cata("</details>")
            cata("</td><td>")
            cata(FormatAsPercent(a), " (", b, ")", sep = "")

            cata("</td></tr>")
        },
        x = variant.counts[names(entity.counts)],
        y = variant.percentages[names(entity.counts)],
        z = names(entity.counts),
        a = entity.percentages,
        b = entity.counts)
    }


    cata("</tbody></table>")

    cata("<div class=\"footer\">", htmlText(footer), "</div>")

    cata("</div>")

    createWidgetFromFile(tfile)
}
