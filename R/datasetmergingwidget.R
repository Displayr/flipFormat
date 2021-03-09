#' @export
DataSetMergingWidget <- function(variable.metadata,
                                 merged.variable.metadata,
                                 merge.map,
                                 merged.data.set.name,
                                 omitted.variables)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("datasetmerging.css", cata)

    cata("<div class=\"data-set-merging-main-container\">")

    cata("<div class=\"data-set-merging-title\">",
         htmlText(merged.data.set.name),
         "</div>")

    # For each variable in the merged data set, create a collapsible container
    # labeled with the variable name and label. The label will be highlighted
    # if there are any irregularities with the merge (e.g. missing variables,
    # inexact matches, conflicting category labels and values, manually
    # specified matches).
    # The container will have a table showing the names and labels of the
    # data sets and also a table showing the category values and labels of the
    # data sets (if categorical). Highlighting will be used to indicate
    # irregularities in the table.

    n.vars <- merged.variable.metadata$n.variables
    n.data.sets <- variable.metadata$n.data.sets
    for (i in seq_len(n.vars))
    {
        var.name <- merged.variable.metadata$variable.names[i]
        var.label <- merged.variable.metadata$variable.labels[i]

        if (var.name != "mergesrc")
        {
            input.var.ind <- vapply(seq_len(n.data.sets), function(j) {
                match(merge.map$input.names[i, j], variable.metadata$variable.names[[j]])
            }, integer(1))

            input.var.names <- vapply(merge.map$input.names[i, ], function(nm) {
                if (!is.na(nm))
                    nm
                else
                    "-"
            }, character(1))

            input.var.labels <- vapply(seq_len(n.data.sets), function(j) {
                ind <- input.var.ind[j]
                if (!is.na(ind))
                    variable.metadata$variable.labels[[j]][ind]
                else
                    "-"
            }, character(1))

            categories <- merged.variable.metadata$variable.categories[[i]]
            if (!is.null(categories))
            {
                categories.table <- matrix(NA_integer_,
                                           nrow = length(categories),
                                           ncol = n.data.sets)
                for (j in seq_len(length(categories)))
                {
                    for (k in seq_len(n.data.sets))
                    {
                        input.categories <- variable.metadata$variable.categories[[k]][[input.var.ind[k]]]
                        ind <- match(names(categories)[j],
                                     names(input.categories))
                        if (!is.na(ind))
                            categories.table[j, k] <- input.categories[ind]
                    }
                }

                # Highlight if names or categories are different
                is.highlighted <- length(unique(input.var.names)) > 1 ||
                                  length(unique(input.var.labels)) > 1 ||
                                  any(apply(categories.table, 1, function(row) length(unique(row))) > 1)
            }
        }
        else
            is.highlighted <- FALSE

        summary.classes <- if (is.highlighted)
            "summary data-set-merging-summary data-set-merging-summary-highlight"
        else
            "summary data-set-merging-summary"

        cata("<details class=\"details data-set-merging-details\">")
        cata("<summary class=\"", summary.classes, "\"><span>",
             paste0(htmlText(var.name), ": ", htmlText(var.label)),
             "</span></summary>")

        if (var.name != "mergesrc")
        {
            # Variable name and label table
            cata("<table class=\"data-set-merging-table data-set-merging-variable-table\"><thead>",
                 "<th>Data set</th><th>Variable name</th><th>Variable label</th></thead><tbody>")
            rows.html <- paste0("<tr><td>", htmlText(merged.data.set.name),
                                "</td><td>", htmlText(var.name),
                                "</td><td>", htmlText(var.label),
                                "</td></tr>")

            for (j in seq_len(n.data.sets))
            {
                name.cell.class <- if (input.var.names[j] != var.name)
                    "data-set-merging-cell-highlight"
                else
                    ""

                label.cell.class <- if (input.var.labels[j] != var.label)
                    "data-set-merging-cell-highlight"
                else
                    ""

                rows.html <- paste0(rows.html,
                                    "<tr><td>",
                                    htmlText(variable.metadata$data.set.names[j]),
                                    "</td><td class=\"", name.cell.class,"\">",
                                    htmlText(input.var.names[j]),
                                    "</td><td class=\"", label.cell.class,"\">",
                                    htmlText(input.var.labels[j]), "</td></tr>")
            }

            cata(rows.html, "</tbody></table>")

            # Categories table
            categories <- merged.variable.metadata$variable.categories[[i]]
            if (!is.null(categories))
            {
                cata("<table class=\"data-set-merging-table data-set-merging-category-table\">",
                     "<thead><th>Category</th><th>",
                     htmlText(merged.data.set.name), "</th>",
                     paste0(paste0("<th>", htmlText(variable.metadata$data.set.names), "</th>"), collapse = ""),
                     "</thead><tbody>")

                rows.html <- ""

                for (j in seq_len(length(categories)))
                {
                    rows.html <- paste0(rows.html, "<tr><td>",
                                        htmlText(names(categories)[j]),
                                        "</td><td>", categories[j], "</td>")

                    for (k in seq_len(n.data.sets))
                    {
                        rows.html <- if (!is.na(categories.table[j, k]))
                        {
                            cell.class <- if (categories.table[j, k] != categories[j])
                                "data-set-merging-cell-highlight"
                            else
                                ""

                            paste0(rows.html, "<td class=\"", cell.class, "\">",
                                   categories.table[j, k], "</td>")
                        }
                        else
                            paste0(rows.html, "<td class=\"data-set-merging-cell-highlight\">-</td>")
                    }
                    rows.html <- paste0(rows.html, "</tr>")
                }
                cata(rows.html, "</tbody></table>")
            }
        }
        else
        {
            "<span>Description of mergesrc here</span>"
        }

        cata("</details>")
    }

    n.omitted <- vapply(omitted.variables, length, integer(1))
    if (any(n.omitted > 0))
    {
        # Additional tables showing omitted variables
        cata("<div class=\"data-set-merging-omitted\">",
             "<div class=\"data-set-merging-title\">",
             "Omitted variables", "</div>")

        for (i in which(n.omitted > 0))
            cata("<div class=\"data-set-merging-subtitle\">",
                 htmlText(paste0(variable.metadata$data.set.names[i], ":")),
                 "</div><table class=\"data-set-merging-table data-set-merging-table-omitted\"><thead>",
                 "<th>Variable name</th><th>Variable label</th></thead><tbody>",
                 paste0(vapply(seq_len(n.omitted[i]), function(j) {
                     nm <- omitted.variables[[i]][j]
                     ind <- match(nm, variable.metadata$variable.names[[i]])
                     lbl <- variable.metadata$variable.labels[[i]][ind]
                     paste0("<tr><td>", htmlText(nm),
                            "</td><td>", htmlText(lbl), "</td></tr>")
                 }, character(1)), collapse = ""), "</tbody></table></div>")
    }
    else
        cata("<div class=\"data-set-merging-omitted\">",
             "<div class=\"data-set-merging-title\">Omitted variables: none</div></div>")

    cata("</div>") # close data-set-merging-main-container

    createWidgetFromFile(tfile)
}
