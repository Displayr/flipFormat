#' @export
StackingWidget <- function(input.data.set.metadata,
                           stacked.data.set.metadata,
                           stacking.groups,
                           stacked.indices,
                           stacked.data.set.name)
{
    n.stacked <- ncol(stacking.groups)
    n.vars <- stacked.data.set.metadata$n.variables
    input.variable.names <- input.data.set.metadata$variable.names
    input.variable.labels <- input.data.set.metadata$variable.labels

    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("stacking.css", cata)

    html <- paste0("<div class=\"stacking-main-container\">",
                   "<div class=\"stacking-title\">",
                   htmlText(stacked.data.set.name), "</div>")

    html.rows <- character(n.vars)
    for (i in seq_len(n.vars))
    {
        stacked.variable.name <- stacked.data.set.metadata$variable.names[i]
        stacked.variable.label <- stacked.data.set.metadata$variable.labels[i]
        row.title <- paste0(stacked.variable.name, ": ",
                            stacked.variable.label)
        if (!(i %in% stacked.indices))
        {
            html.row <- paste0("<div class=\"stacking-row\">",
                               htmlText(row.title), "</div>")
        }
        else
        {
            ind <- match(i, stacked.indices)
            table.html <- stackingTable(stacking.groups[ind, ], n.stacked,
                                        input.variable.names,
                                        input.variable.labels,
                                        stacked.variable.name,
                                        stacked.variable.label)

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

    unstackable.ind <- attr(stacking.groups, "unstackable.ind")
    unstackable.names <- lapply(unstackable.ind, function(ind) {
        group.ind <- stacking.groups[ind, ]
        group.ind <- group.ind[!is.na(group.ind)]
        input.variable.names[group.ind]
    })
    for (nms in unstackable.names)
        html <- paste0(html, "<div>The following variables could not be ",
                       "stacked due to mismatching variable types or ",
                       "categories: ", paste0("'", nms, "'", collapse = ", "),
                       ".</div>")

    html <- paste0(html, "</div>")

    cata(html)

    createWidgetFromFile(tfile)
}

stackingTable <- function(stacking.group, n.stacked, variable.names, variable.labels,
                          stacked.variable.name, stacked.variable.label)
{
    table.html <- paste0("<table class=\"stacking-table\">",
                         "<th></th><th>Name</th><th>Label</th>",
                         "<tr><td>Stacked</td><td>",
                         htmlText(stacked.variable.name), "</td><td>",
                         htmlText(stacked.variable.label), "</td></tr>")

    table.html <- paste0(table.html,
                         paste0(vapply(seq_along(stacking.group), function(j) {
        if (is.na(stacking.group[j]))
            paste0("<tr><td>Observation ", j, "</td><td></td><td></td></tr>")
        else
            paste0("<tr><td>Observation ", j, "</td><td>",
                   htmlText(variable.names[stacking.group[j]]), "</td><td>",
                   htmlText(variable.labels[stacking.group[j]]), "</td></tr>")
    }, character(1)), collapse = ""))

    paste0(table.html, "</table></details>")
}
