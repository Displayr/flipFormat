#' @export
StackingWidget <- function(stacked.variable.names,
                           stacked.variable.labels,
                           stacking.array)
{
    n.stacked <- dim(stacking.array)[2]
    n.vars <- length(stacked.variable.names)

    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("stacking.css", cata)

    html <- paste0("<div class=\"stacking-main-container\">")

    html.rows <- character(n.vars)
    for (i in seq_len(n.vars))
    {
        row.title <- paste0(stacked.variable.names[i], ": ",
                            stacked.variable.labels[i])
        if (length(unique(stacking.array[i, , 1])) == 1)
        {
            html.row <- paste0("<div class=\"stacking-row\">", row.title, "</div>")
        }
        else
        {
            html.row <- paste0("<details class=\"stacking-details\">",
                               "<summary class=\"stacking-summary\">",
                               row.title, "</summary>",
                               "<table class=\"stacking-table\">",
                               "<th></th><th>Name</th><th>Label</th>")

            html.row <- paste0(html.row, paste0(vapply(seq_len(n.stacked), function(j) {
                if (is.na(stacking.array[i, j, 1]))
                    paste0("<tr><td>Observation ", j, "</td><td></td><td></td></tr>")
                else
                    paste0("<tr><td>Observation ", j, "</td><td>",
                           stacking.array[i, j, 1], "</td><td>",
                           stacking.array[i, j, 2], "</td></tr>")
            }, character(1)), collapse = ""))

            html.row <- paste0(html.row, "</table></details>")
        }
        html.rows[i] <- html.row
    }
    html <- paste0(html, paste0(html.rows, collapse = ""))

    # html <- paste0("<div class=\"stacking-main-container\">",
    #                "<table class=\"stacking-table\"><thead>",
    #                "<th colspan=", n.stacked, ">Stacked Variables</th>",
    #                "<th>Name</th><th>Label</th></thead>")
    #
    # html.rows <- character(n.vars)
    # for (i in seq_len(n.vars))
    # {
    #     html.row <- "<tr>"
    #     for (j in seq_len(n.stacked))
    #     {
    #         html.row <- if (is.na(stacking.table[i, j]))
    #             paste0(html.row, "<td></td>")
    #         else
    #             paste0(html.row, "<td>", stacking.table[i, j], "</td>")
    #         paste0(html.row, "<td>", stacked.variable.names[i], "</td>")
    #         paste0(html.row, "<td>", stacked.variable.labels[i], "</td>")
    #     }
    #     paste0(html.row, "</tr>")
    #     html.rows[i] <- html.row
    # }
    #
    # html <- paste0(html, paste0(html.rows, collapse = ""))

    html <- paste0(html, "</div>")

    cata(html)

    createWidgetFromFile(tfile)
}
