#' @export
DataSetMergingWidget <- function(variable.metadata,
                                 merged.variable.metadata,
                                 merge.map,
                                 merged.data.set.name,
                                 omitted.variables,
                                 input.category.values)
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
        var.type <- variableTypeConverter(merged.variable.metadata$variable.types[i])
        merged.categories <- merged.variable.metadata$variable.categories[[i]]

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

            input.var.types <- vapply(seq_len(n.data.sets), function(j) {
                ind <- input.var.ind[j]
                if (!is.na(ind))
                    variableTypeConverter(variable.metadata$variable.types[[j]][ind])
                else
                    "-"
            }, character(1))

            variable.table.html <- inputVariableTable(merged.data.set.name,
                                                      var.name, var.label,
                                                      var.type, input.var.names,
                                                      input.var.labels,
                                                      input.var.types,
                                                      variable.metadata$data.set.names)
            html.row <- variable.table.html
            is.summary.highlighted <- attr(variable.table.html,
                                           "is.summary.highlighted")

            if (!is.null(merged.categories))
            {
                categories.table.html <- categoriesTable(merged.categories,
                                                         merged.data.set.name,
                                                         variable.metadata,
                                                         input.category.values[[i]],
                                                         input.var.ind)
                html.row <- paste0(html.row, categories.table.html)
                is.summary.highlighted <- is.summary.highlighted ||
                                          attr(categories.table.html,
                                               "is.summary.highlighted")
            }
        }
        else # mergesrc variable
        {
            html.row <- mergesrcCategoriesTable(merged.categories,
                                                merged.data.set.name)
            is.summary.highlighted <- FALSE
            input.var.names <- NULL
        }

        # Create details summary last since it is easier to determine if it
        # needs to be highlighted
        html.summary <- variableSummary(var.name, var.label,
                                        is.summary.highlighted,
                                        num.span.width, n.data.sets,
                                        input.var.names, i)

        html.vars[i] <- paste0("<details class=\"details data-set-merging-details\">",
                               html.summary, html.row, "</details>")
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
                 }, character(1)), collapse = ""), "</tbody></table>")
        html <- paste0(html, "</div>")
    }
    else
        html <- paste0(html, "<div class=\"data-set-merging-omitted\">",
                       "<div class=\"data-set-merging-title\">Omitted variables: none</div></div>")

    html <- paste0(html, "<div class=\"data-set-merging-title\">",
                   "Note:", "</div>")

    unmatched.names <- merge.map$unmatched.names
    unmatched.names.renamed <- merge.map$unmatched.names.renamed

    for (i in seq_len(nrow(unmatched.names)))
    {
        nms <- unmatched.names[i, ]
        nms <- nms[!is.na(nms)]
        nms.str <- paste0(paste0("<b>", nms, "</b>"), collapse = ", ")

        note.html <- paste0("<div>The variables ", nms.str,
                            " could not be merged due to incompatible ",
                            "variable types.")

        renamed <- unmatched.names.renamed[[i]]
        if (!is.null(renamed))
        {
            renamed.str <- paste0(paste0("<b>", renamed, "</b>"), collapse = ", ")
            note.html <- paste0(note.html, " The following variable",
                                ngettext(length(renamed), "", "s"),
                                " had to be created to avoid conflicting names: ",
                                renamed.str, "</div>")
        }
        else
            note.html <- paste0(note.html, "</div>")

        html <- paste0(html, note.html)
    }

    converted.var <- convertedVariables(variable.metadata,
                                        merged.variable.metadata, merge.map)
    for (i in seq_len(nrow(converted.var)))
    {
        r <- converted.var[i, ]
        html <- paste0(html, "<div>Variable <b>", r[1], "</b> (", r[5], ") from <i>",
                       r[2],"</i> converted from ", r[3], " to ", r[4], ".</div>")
    }

    html <- paste0(html, "</div>") # close data-set-merging-main-container
    cata(html)

    createWidgetFromFile(tfile)
}

convertedVariables <- function(variable.metadata, merged.variable.metadata, merge.map)
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
                result <- rbind(result, c(merge.map$input.names[i, j],
                                          variable.metadata$data.set.names[j],
                                          t, merged.type, i))
        }
    }
    result
}

# Convert from internal types to user-facing types
variableTypeConverter <- function(variable.type)
{
    if (variable.type == "Categorical with string values")
        "Categorical"
    else if (variable.type == "Duration")
        "Date/Time"
    else
        variable.type
}

variableSummary <- function(var.name, var.label, is.summary.highlighted,
                            num.span.width, n.data.sets, input.var.names,
                            variable.index)
{
    summary.classes <- if (is.summary.highlighted)
        "summary data-set-merging-summary data-set-merging-summary-highlight"
    else
        "summary data-set-merging-summary"

    name.and.label <- if (nchar(var.label) > 0)
    {
        if (substr(var.label, 1, nchar(var.name) + 1) == paste0(var.name, ":") ||
            var.name == var.label)
            htmlText(var.label)
        else
            paste0(htmlText(var.name), ": ", htmlText(var.label))
    }
    else
        htmlText(var.name)

    result <- paste0("<summary class=\"", summary.classes, "\">",
                           "<span class=\"data-set-merging-var-num\" style=\"width:",
                           num.span.width, "px\">", variable.index, ".</span>")

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
        rep("<span class=\"data-set-merging-indicator-mergesrc\">&#8193;</span>", n.data.sets)

    paste0(result, "<span class=\"data-set-merging-indicator-container\">",
           paste0(indicators, collapse = ""), "</span>", name.and.label,
           "</summary>")
}

inputVariableTable <- function(merged.data.set.name, var.name, var.label,
                               var.type, input.var.names, input.var.labels,
                               input.var.types, data.set.names)
{
    result <- paste0("<table class=\"data-set-merging-table data-set-merging-variable-table\"><thead>",
                     "<th>Data set</th><th>Variable name</th>",
                     "<th>Variable label</th><th>Variable type</th>",
                     "</thead><tbody>")

    result <- paste0(result,
                     "<tr><td>", htmlText(merged.data.set.name),
                     "</td><td>", htmlText(var.name),
                     "</td><td>", htmlText(var.label),
                     "</td><td>", htmlText(var.type),
                     "</td></tr>")

    is.summary.highlighted <- FALSE
    for (j in seq_along(data.set.names))
    {
        name.cell.class <- if (input.var.names[j] != var.name)
        {
            is.summary.highlighted <- TRUE
            "data-set-merging-cell-highlight"
        }
        else
            ""

        label.cell.class <- if (input.var.labels[j] != var.label)
        {
            is.summary.highlighted <- TRUE
            "data-set-merging-cell-highlight"
        }
        else
            ""

        type.cell.class <- if (input.var.types[j] != var.type)
        {
            is.summary.highlighted <- TRUE
            "data-set-merging-cell-highlight"
        }
        else
            ""

        result <- paste0(result,
                         "<tr><td>",
                         htmlText(data.set.names[j]),
                         "</td><td class=\"", name.cell.class,"\">",
                         htmlText(input.var.names[j]),
                         "</td><td class=\"", label.cell.class,"\">",
                         htmlText(input.var.labels[j]),
                         "</td><td class=\"", type.cell.class,"\">",
                         htmlText(input.var.types[j]),
                         "</td></tr>")
    }

    result <- paste0(result, "</tbody></table>")
    attr(result, "is.summary.highlighted") <- is.summary.highlighted
    result
}

categoriesTable <- function(merged.categories, merged.data.set.name,
                            variable.metadata, input.categories.list,
                            input.var.ind)
{
    cat.types <- c("Categorical", "Categorical with string values")
    input.var.types <- variable.metadata$variable.types
    input.var.categories <- variable.metadata$variable.categories

    result <- paste0("<table class=\"data-set-merging-table data-set-merging-category-table\">",
                     "<thead><th colspan=2 style=\"text-align:center\">",
                     htmlText(merged.data.set.name), "</th>",
                     paste0(paste0("<th>", htmlText(variable.metadata$data.set.names),
                                   "</th>"), collapse = ""), "</thead><tbody>")

    is.summary.highlighted <- FALSE
    for (j in seq_len(length(merged.categories)))
    {
        result <- paste0(result, "<tr><td>",
                         htmlText(names(merged.categories)[j]),
                         "</td><td>", htmlText(merged.categories[j]), "</td>")

        for (k in seq_len(variable.metadata$n.data.sets))
        {
            result <- if (!is.na(input.categories.list[[k]][j]))
            {
                val <- input.categories.list[[k]][j]

                if (input.var.types[[k]][input.var.ind[k]] %in% cat.types)
                {
                    categories <- input.var.categories[[k]][[input.var.ind[k]]]
                    lbl <- names(categories)[categories == input.categories.list[[k]][j]]
                    cell.class <- if (input.categories.list[[k]][j] != merged.categories[j] ||
                                      lbl != names(merged.categories)[j])
                    {
                        is.summary.highlighted <- TRUE
                        "data-set-merging-cell-highlight"
                    }
                    else
                        ""

                    paste0(result, "<td title=\"", htmlText(val),
                           ": ", htmlText(lbl), "\" class=\"", cell.class, "\">",
                           htmlText(val), "</td>")
                }
                else # Numeric or Text variable
                {
                    is.summary.highlighted <- TRUE
                    paste0(result, "<td title=\"From ",
                           input.var.types[[k]][input.var.ind[k]],
                           " variable\" class=\"data-set-merging-cell-highlight\">",
                           htmlText(val), "</td>")
                }
            }
            else # Missing value
            {
                is.summary.highlighted <- TRUE
                paste0(result, "<td class=\"data-set-merging-cell-highlight\">-</td>")
            }
        }
        result <- paste0(result, "</tr>")
    }
    result <- paste0(result, "</tbody></table>")
    attr(result, "is.summary.highlighted") <- is.summary.highlighted
    result
}

mergesrcCategoriesTable <- function(merged.categories, merged.data.set.name)
{
    # Categories table
    # merged.categories <- merged.variable.metadata$variable.categories[[i]]
    result <- paste0("<table class=\"data-set-merging-table data-set-merging-category-table\">",
                     "<thead><th>Category</th><th>",
                     htmlText(merged.data.set.name), "</th>",
                     "</thead><tbody>")

    for (j in seq_len(length(merged.categories)))
        result <- paste0(result, "<tr><td>",
                         htmlText(names(merged.categories)[j]),
                         "</td><td>", merged.categories[j], "</td></tr>")
    paste0(result, "</tbody></table>")
}
