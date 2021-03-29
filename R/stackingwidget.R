#' @export
StackingWidget <- function(stacked.data.set.metadata,
                           unstackable.names,
                           omitted.variables,
                           omitted.stacked.variables)
{
    md <- stacked.data.set.metadata

    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("stacking.css", cata)

    html <- paste0("<div class=\"stacking-main-container\">",
                   "<div class=\"stacking-title\">",
                   htmlText(md$data.set.name),
                   "</div><div class=\"stacking-subtitle\">",
                   md$n.variables,
                   " variables, ",
                   sum(md$is.stacked.variable) - sum(md$is.manually.stacked.variable, na.rm = TRUE),
                   " variables stacked using common labels, ",
                   sum(md$is.manually.stacked.variable, na.rm = TRUE),
                   " manually stacked variables")

    num.span.width <- ceiling(log10(md$n.variables + 1)) * 10 + 15

    html.rows <- character(md$n.variables)
    for (i in seq_len(md$n.variables))
    {
        row.title <- paste0(md$variable.names[i], ": ",
                            md$variable.labels[i])
        if (!(md$is.stacked.variable[i]))
        {
            html.row <- paste0("<div class=\"stacking-row\">",
                               "<span class=\"stacking-var-num\" style=\"width:",
                               num.span.width, "px\">", i, ".</span>",
                               htmlText(row.title), "</div>")
        }
        else
        {
            table.html <- stackingTable(md, i)

            summary.class <- if (md$is.manually.stacked[i])
                "stacking-summary stacking-summary-manual"
            else
                "stacking-summary"

            html.row <- paste0("<details class=\"stacking-details\">",
                               "<summary class=\"", summary.class, "\">",
                               "<span class=\"stacking-var-num\" style=\"width:",
                               num.span.width, "px\">", i, ".</span>",
                               htmlText(row.title),
                               "</summary>",
                               table.html, "</table></details>")
        }
        html.rows[i] <- html.row
    }
    html <- paste0(html, paste0(html.rows, collapse = ""))

    # Whether to show Note
    if (length(unstackable.names) > 0 ||
        length(omitted.variables) > 0)
    {
        html <- paste0(html, "<div class=\"stacking-title\">",
                       "Note:", "</div>")

        html <- paste0(html, paste0(vapply(unstackable.names, function(nms) {
            paste0("<div class=\"stacking-note\">The following variables could not be ",
                   "stacked due to mismatching variable types or ",
                   "categories: ", paste0("'", nms, "'", collapse = ", "),
                   ".</div>")
        }, character(1)), collapse = ""))


        if (length(omitted.stacked.variables) > 0)
            html <- paste0(html, "<div class=\"stacking-note\">The following <b>stacked</b> variable",
                           ngettext(length(omitted.stacked.variables), " has", "s have"),
                           " been omitted from the stacked data set: ",
                           paste0("<b>", omitted.stacked.variables, "</b>", collapse = ", "),
                           ".</div>")

        omitted.non.stacked.variables <- setdiff(omitted.variables,
                                                 omitted.stacked.variables)

        if (length(omitted.non.stacked.variables) > 0)
            html <- paste0(html, "<div class=\"stacking-note\">The following variable",
                           ngettext(length(omitted.non.stacked.variables), " has", "s have"),
                           " been omitted from the stacked data set: ",
                           paste0("<b>", omitted.non.stacked.variables, "</b>", collapse = ", "),
                           ".</div>")
    }
    html <- paste0(html, "</div>")

    cata(html)

    createWidgetFromFile(tfile)
}

stackingTable <- function(stacked.data.set.metadata, var.ind)
{
    md <- stacked.data.set.metadata

    table.html <- paste0("<table class=\"stacking-table\"><thead>",
                         "<th></th><th>Name</th><th>Label</th></thead>",
                         "<tbody><tr><td>Stacked</td><td>",
                         htmlText(md$variable.names[var.ind]), "</td><td>",
                         htmlText(md$variable.labels[var.ind]), "</td></tr>")

    stacking.input.var.names <- md$stacking.input.variable.names[[var.ind]]
    stacking.input.var.labels <- md$stacking.input.variable.labels[[var.ind]]

    table.html <- paste0(table.html,
                         paste0(vapply(seq_along(stacking.input.var.names), function(j) {
        if (is.na(stacking.input.var.names[j]))
            paste0("<tr><td>Observation ", j, "</td><td></td><td></td></tr>")
        else
            paste0("<tr><td>Observation ", j, "</td><td>",
                   htmlText(stacking.input.var.names[j]), "</td><td>",
                   htmlText(stacking.input.var.labels[j]), "</td></tr>")
    }, character(1)), collapse = ""))

    paste0(table.html, "</tbody></table></details>")
}
