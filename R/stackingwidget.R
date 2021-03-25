#' @export
StackingWidget <- function(stacked.data.set.metadata,
                           unstackable.names)
{
    md <- stacked.data.set.metadata

    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("stacking.css", cata)

    html <- paste0("<div class=\"stacking-main-container\">",
                   "<div class=\"stacking-title\">",
                   htmlText(md$data.set.name),
                   "</div>")

    html.rows <- character(md$n.variables)
    for (i in seq_len(md$n.variables))
    {
        row.title <- paste0(md$variable.names[i], ": ",
                            md$variable.labels[i])
        if (!(md$is.stacked.variable[i]))
        {
            html.row <- paste0("<div class=\"stacking-row\">",
                               htmlText(row.title), "</div>")
        }
        else
        {
            table.html <- stackingTable(md, i)

            html.row <- paste0("<details class=\"stacking-details\">",
                               "<summary class=\"stacking-summary\">",
                               htmlText(row.title), "</summary>",
                               table.html, "</table></details>")
        }
        html.rows[i] <- html.row
    }
    html <- paste0(html, paste0(html.rows, collapse = ""))

    html <- paste0(html, "<div class=\"stacking-title\">",
                   "Note:", "</div>")

    for (nms in unstackable.names)
        html <- paste0(html, "<div>The following variables could not be ",
                       "stacked due to mismatching variable types or ",
                       "categories: ", paste0("'", nms, "'", collapse = ", "),
                       ".</div>")

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
    stacking.input.var.labels <- md$stacking.input.variable.names[[var.ind]]

    table.html <- paste0(table.html,
                         paste0(vapply(seq_along(stacking.group), function(j) {
        if (is.na(stacking.input.var.names[j]))
            paste0("<tr><td>Observation ", j, "</td><td></td><td></td></tr>")
        else
            paste0("<tr><td>Observation ", j, "</td><td>",
                   htmlText(stacking.input.var.names[j]), "</td><td>",
                   htmlText(stacking.input.var.labels[j]), "</td></tr>")
    }, character(1)), collapse = ""))

    paste0(table.html, "</tbody></table></details>")
}
