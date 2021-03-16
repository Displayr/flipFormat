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

    html <- paste0("<div class=\"data-set-merging-main-container\">",
                   "<div class=\"data-set-merging-title\">",
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
    num.span.width <- ceiling(log10(n.vars + 1)) * 10 + 15

    html.vars <- rep(NA_character_, n.vars)

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

            is.highlighted <- length(unique(input.var.names)) > 1 ||
                              length(unique(input.var.labels)) > 1

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
                is.highlighted <- is.highlighted ||
                                  any(apply(categories.table, 1, function(row) length(unique(row))) > 1)
            }
        }
        else
            is.highlighted <- FALSE

        summary.classes <- if (is.highlighted)
            "summary data-set-merging-summary data-set-merging-summary-highlight"
        else
            "summary data-set-merging-summary"

        name.and.label <- if (nchar(var.label) > 0)
            paste0(htmlText(var.name), ": ", htmlText(var.label))
        else
            htmlText(var.name)

        html.row <- ""
        html.row <- paste0(html.row,
                           "<details class=\"details data-set-merging-details\">",
                           "<summary class=\"", summary.classes, "\">",
                           "<span class=\"data-set-merging-var-num\" style=\"width:",
                           num.span.width, "px\">", i, ".</span>")

        indicators <- if (var.name != "mergesrc")
        {
            vapply(seq_len(n.data.sets), function(j) {
                if (input.var.names[j] != "-")
                    "<span class=\"data-set-merging-indicator data-set-merging-indicator-fill\">&#8193;</span>"
                else
                    "<span class=\"data-set-merging-indicator\">&#8193;</span>"
            }, character(1))
        }
        else
            rep("<span class=\"data-set-merging-indicator\">&#8193;</span>", n.data.sets)

        html.row <- paste0(html.row, "<span class=\"data-set-merging-indicator-container\">",
                           paste0(indicators, collapse = ""), "</span>", name.and.label,
                           "</summary>")


        if (var.name != "mergesrc")
        {
            t <- merged.variable.metadata$variable.types[i]
            if (t == "Categorical with string values")
                t <- "Categorical"
            else if (t == "Duration")
                t <- "Date/Time"

            html.row <- paste0(html.row,
                               "<div class=\"data-set-merging-type\"><b>Type:</b> ",
                               t, "</div>")

            # Variable name and label table
            html.row <- paste0(html.row, "<table class=\"data-set-merging-table data-set-merging-variable-table\"><thead>",
                 "<th>Data set</th><th>Variable name</th><th>Variable label</th></thead><tbody>")

            html.row <- paste0(html.row,
                           "<tr><td><i>", htmlText(merged.data.set.name),
                           "</i></td><td>", htmlText(var.name),
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

                html.row <- paste0(html.row,
                               "<tr><td>",
                               htmlText(variable.metadata$data.set.names[j]),
                               "</td><td class=\"", name.cell.class,"\">",
                               htmlText(input.var.names[j]),
                               "</td><td class=\"", label.cell.class,"\">",
                               htmlText(input.var.labels[j]), "</td></tr>")
            }

            html.row <- paste0(html.row, "</tbody></table>")

            # Categories table
            categories <- merged.variable.metadata$variable.categories[[i]]
            if (!is.null(categories))
            {
                html.row <- paste0(html.row, "<table class=\"data-set-merging-table data-set-merging-category-table\">",
                     "<thead><th>Category</th><th>",
                     htmlText(merged.data.set.name), "</th>",
                     paste0(paste0("<th>", htmlText(variable.metadata$data.set.names), "</th>"), collapse = ""),
                     "</thead><tbody>")

                for (j in seq_len(length(categories)))
                {
                    html.row <- paste0(html.row, "<tr><td>",
                                        htmlText(names(categories)[j]),
                                        "</td><td>", htmlText(categories[j]), "</td>")

                    for (k in seq_len(n.data.sets))
                    {
                        html.row <- if (!is.na(categories.table[j, k]))
                        {
                            cell.class <- if (categories.table[j, k] != categories[j])
                                "data-set-merging-cell-highlight"
                            else
                                ""

                            paste0(html.row, "<td class=\"", cell.class, "\">",
                                   htmlText(categories.table[j, k]), "</td>")
                        }
                        else
                            paste0(html.row, "<td class=\"data-set-merging-cell-highlight\">-</td>")
                    }
                    html.row <- paste0(html.row, "</tr>")
                }
                html.row <- paste0(html.row, "</tbody></table>")
            }
        }
        else
        {
            # Categories table
            categories <- merged.variable.metadata$variable.categories[[i]]
            html.row <- paste0(html.row, "<table class=\"data-set-merging-table data-set-merging-category-table\">",
                 "<thead><th>Category</th><th>",
                 htmlText(merged.data.set.name), "</th>",
                 "</thead><tbody>")

            for (j in seq_len(length(categories)))
                html.row <- paste0(html.row, "<tr><td>",
                               htmlText(names(categories)[j]),
                               "</td><td>", categories[j], "</td></tr>")
            html.row <- paste0(html.row, "</tbody></table>")
        }

        html.row <- paste0(html.row, "</details>")
        html.vars[i] <- html.row
    }

    html <- paste0(html, paste0(html.vars, collapse = ""))

    n.omitted <- vapply(omitted.variables, length, integer(1))
    if (any(n.omitted > 0))
    {
        # Additional tables showing omitted variables
        html <- paste0(html, "<div class=\"data-set-merging-omitted\">",
             "<div class=\"data-set-merging-title\">",
             "Omitted variables", "</div>")

        for (i in which(n.omitted > 0))
            html <- paste0(html, "<div class=\"data-set-merging-subtitle\">",
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
        html <- paste0(html, "<div class=\"data-set-merging-omitted\">",
                       "<div class=\"data-set-merging-title\">Omitted variables: none</div></div>")

    html <- paste0(html, "<div class=\"data-set-merging-title\">",
                   "Note:", "</div>")
    dedup <- merge.map$deduplicated.names
    for (i in seq_len(nrow(dedup)))
    {
        ind <- match(dedup[i, 2], merged.variable.metadata$variable.names)
        html <- paste0(html, "<div>Variable <b>",
                       dedup[i, 2], "</b> (", ind, ") was created because the variables named <b>",
                       dedup[i, 1], "</b> could not be merged due to incompatible variable types.</div>")
    }


    converted.text.var <- convertedTextVariables(variable.metadata,
                                                 merged.variable.metadata,
                                                 merge.map)
    for (i in seq_len(nrow(converted.text.var)))
    {
        r <- converted.text.var[i, ]
        html <- paste0(html, "<div>Variable <b>", r[1], "</b> (", r[5], ") from <i>",
                       r[2],"</i> converted from ", r[3], " to ", r[4], ".</div>")
    }

    html <- paste0(html, "</div>") # close data-set-merging-main-container
    cata(html)

    createWidgetFromFile(tfile)
}

convertedTextVariables <- function(variable.metadata, merged.variable.metadata, merge.map)
{
    n.var <- length(merge.map$merged.names)
    n.data.sets <- variable.metadata$n.data.sets
    result <- matrix(nrow = 0, ncol = 5)
    for (i in seq_len(n.var))
    {
        merged.type <- merged.variable.metadata$variable.types[i]
        if (merged.type == "Categorical with string values")
            merged.type <- "Categorical"
        for (j in seq_len(n.data.sets))
        {
            if (is.na(merge.map$input.names[i, j]))
                next

            ind <- which(variable.metadata$variable.names[[j]] == merge.map$input.names[i, j])
            t <- variable.metadata$variable.types[[j]][ind]
            if ((t == "Text" || t == "Numeric") && t != merged.type)
            {
                result <- rbind(result, c(merge.map$input.names[i, j],
                                          variable.metadata$data.set.names[j],
                                          t, merged.type, i))
            }
        }
    }
    result
}
