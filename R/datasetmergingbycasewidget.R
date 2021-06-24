#' @title Widget Output for Data Set Merging By Case
#' @description Widget shown in output for \code{flipData::MergeDataSetsByCase}.
#' @param input.data.sets.metadata An object containing metadata for the
#'   input data set.
#' @param merged.data.set.metadata An object containing metadata for the
#'   merged data set.
#' @param matched.names A matrix whose rows contain the names of the input
#'   variables used to create the merged variables. This matrix also has the
#'   attributes "is.fuzzy.match" and "matched.by" which are matrices indicating
#'   whether the input variable was a fuzzy match and what they were matched by,
#'   i.e., variable name, label or value label.
#' @param merged.names A character vector of the names of the merged variables.
#'   Also contains the attribute "renamed.variables" which contains the
#'   merged variables that were renamed due to name conflicts and their
#'   original names.
#' @param omitted.variable.names A list where each element contains the names
#'   of the variables omitted from a data set.
#' @param input.value.attributes A list where each element contains the value
#'   attributes of input categorical variables from a data set.
#' @param is.saved.to.cloud Whether the merged data set was saved to the
#'   Displayr cloud drive.
#' @export
DataSetMergingByCaseWidget <- function(input.data.sets.metadata,
                                       merged.data.set.metadata,
                                       matched.names,
                                       merged.names,
                                       omitted.variable.names,
                                       input.value.attributes,
                                       is.saved.to.cloud)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("datasetwidget.css", cata)
    addCss("datasetmerging.css", cata)

    is.fuzzy.match <- attr(matched.names, "is.fuzzy.match")
    vars.matched.by <- attr(matched.names, "matched.by")

    html <- paste0("<div class=\"data-set-widget-main-container\">",
                   "<div class=\"data-set-widget-title\">",
                   htmlText(merged.data.set.metadata$data.set.name),
                   "</div>")

    html <- paste0(html, mergingSubtitle(merged.data.set.metadata, vars.matched.by,
                                         is.saved.to.cloud))

    # For each variable in the merged data set, create a collapsible container
    # labeled with the variable name and label. The label will be highlighted
    # if there are any irregularities with the merge (e.g. missing variables,
    # inexact matches, conflicting values, manually specified matches).
    # The container will have a table showing the names and labels of the
    # data sets and also a table showing the values and value labels of the
    # data sets (if categorical). Highlighting will be used to indicate
    # irregularities in the table.

    n.vars <- merged.data.set.metadata$n.variables
    n.data.sets <- input.data.sets.metadata$n.data.sets
    num.span.width <- ceiling(log10(n.vars + 1)) * 10 + 15

    renamed.vars <- attr(merged.names, "renamed.variables")
    renamed.vars.original.names <- vapply(renamed.vars, `[[`, character(1), 1)
    renamed.vars.new.names <- vapply(renamed.vars, `[[`, character(1), 2)

    html.vars <- rep(NA_character_, n.vars)

    for (i in seq_len(n.vars))
    {
        var.name <- merged.data.set.metadata$variable.names[i]
        var.label <- merged.data.set.metadata$variable.labels[i]
        var.type <- variableTypeConverter(merged.data.set.metadata$variable.types[i])
        merged.val.attr <- merged.data.set.metadata$variable.value.attributes[[i]]

        if (i < n.vars) # not the mergesrc variable
        {
            input.var.ind <- vapply(seq_len(n.data.sets), function(j) {
                match(matched.names[i, j], input.data.sets.metadata$variable.names[[j]])
            }, integer(1))

            input.var.names <- vapply(matched.names[i, ], function(nm) {
                if (!is.na(nm))
                    nm
                else
                    "-"
            }, character(1))

            input.var.labels <- vapply(seq_len(n.data.sets), function(j) {
                ind <- input.var.ind[j]
                if (!is.na(ind))
                    input.data.sets.metadata$variable.labels[[j]][ind]
                else
                    "-"
            }, character(1))

            input.var.types <- vapply(seq_len(n.data.sets), function(j) {
                ind <- input.var.ind[j]
                if (!is.na(ind))
                    variableTypeConverter(input.data.sets.metadata$variable.types[[j]][ind])
                else
                    "-"
            }, character(1))

            html.row <- ""

            renamed.ind <- match(var.name, renamed.vars.new.names)
            if (!is.na(renamed.ind))
                html.row <- paste0(html.row,
                                   "<span class=\"data-set-widget-subtitle\">Renamed from ",
                                   htmlText(renamed.vars.original.names[renamed.ind]),
                                   "</span>")

            variable.table.html <- inputVariableTable(var.name, var.label,
                                                      var.type, input.var.names,
                                                      input.var.labels,
                                                      input.var.types,
                                                      is.fuzzy.match[i, ],
                                                      vars.matched.by[i, ],
                                                      n.data.sets)
            html.row <- paste0(html.row, variable.table.html)
            is.summary.highlighted <- attr(variable.table.html,
                                           "is.summary.highlighted")

            if (!is.null(merged.val.attr))
            {
                val.attr.table.html <- valueAttributesTable(merged.val.attr,
                                                            input.data.sets.metadata,
                                                            input.value.attributes[[i]],
                                                            input.var.ind)
                html.row <- paste0(html.row, val.attr.table.html)
                is.summary.highlighted <- is.summary.highlighted ||
                                          attr(val.attr.table.html,
                                               "is.summary.highlighted")
            }

            # Only create details summary (expandable info) if highlighted
            # as it is not so interesting otherwise and this helps to reduce
            # the widget size for large data sets
            if (is.summary.highlighted)
            {
                # Create details summary last since it is easier to determine if it
                # needs to be highlighted
                contains.fuzzy.match <- any(is.fuzzy.match[i, ])
                contains.manual.match <- any(!is.na(vars.matched.by[i, ]) &
                                             vars.matched.by[i, ] == "Manual")
                html.summary <- variableSummary(var.name, var.label,
                                                num.span.width, n.data.sets,
                                                input.var.names, i,
                                                contains.fuzzy.match,
                                                contains.manual.match)
                html.vars[i] <- paste0("<details class=\"details data-set-merging-details\">",
                                       html.summary, html.row, "</details>")
            }
            else
            {
                v.index.text <- variableIndexText(i, num.span.width)
                indicators <- dataSetIndicators(input.var.names == "-")
                name.and.label <- variableNameAndLabelText(var.name, var.label)
                html.vars[i] <- paste0("<div class=\"data-set-widget-row\">",
                                       v.index.text, indicators,
                                       name.and.label, "</div>")
            }
        }
        else # mergesrc variable
        {
            v.index.text <- variableIndexText(i, num.span.width)
            indicator.spacing <- paste0("<span class=\"data-set-merging-indicator-container\">",
                                        paste0(rep("<span class=\"data-set-merging-indicator-mergesrc\">&#8193;</span>",
                                               n.data.sets), collapse = ""), "</span>")
            html.vars[i] <- paste0("<div class=\"data-set-widget-row\">",
                                   v.index.text, indicator.spacing,
                                   variableNameAndLabelText(var.name, var.label),
                                   "</div>")
        }
    }

    cata(paste0(html,
                paste0(html.vars, collapse = ""),
                mergingNote(omitted.variable.names),
                "</div>")) # close data-set-merging-main-container

    createWidgetFromFile(tfile)
}

mergingSubtitle <- function(merged.data.set.metadata, vars.matched.by,
                            is.saved.to.cloud)
{
    html <- ""

    if (is.saved.to.cloud)
        html <- paste0(html, "<div class=\"data-set-widget-subtitle\">(saved to Displayr cloud drive)</div>")

    html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                   merged.data.set.metadata$n.variables, " variables, ",
                   merged.data.set.metadata$n.cases, " cases</div>")

    unique.matched.by <- unique(c(vars.matched.by))

    matched.by <- c()
    if ("Variable name" %in% unique.matched.by)
        matched.by <- c(matched.by, "variable names")
    if ("Variable label" %in% unique.matched.by)
        matched.by <- c(matched.by, "variable labels")
    if ("Value label" %in% unique.matched.by)
        matched.by <- c(matched.by, "value labels")

    matched.by.msg <- if ("Manual" %in% unique.matched.by)
        "Matched manually and by "
    else
        "Matched by "

    matched.by.msg <- if (length(matched.by) == 1)
        paste0(matched.by.msg, matched.by)
    else
        paste0(matched.by.msg, paste0(matched.by[-length(matched.by)], collapse = ", "),
               " and ", matched.by[length(matched.by)])

    html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                   matched.by.msg, "</div>")

    html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                   "Legend: ",
                   "<span class=\"data-set-widget-manual-shade\">",
                   "&nbsp;Manual&nbsp;match&nbsp;</span>&nbsp;&nbsp;",
                   "<span class=\"data-set-widget-fuzzy-shade\">",
                   "&nbsp;Fuzzy&nbsp;match&nbsp;</span>&nbsp;&nbsp;",
                   "<span class=\"data-set-widget-highlight-shade\">",
                   "&nbsp;Difference&nbsp;in&nbsp;inputs&nbsp;</span>",
                   "</div>")

    html
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

variableSummary <- function(var.name, var.label, num.span.width, n.data.sets,
                            input.var.names, variable.index,
                            contains.fuzzy.match, contains.manual.match)
{
    summary.classes <- if (contains.manual.match)
        "summary data-set-merging-summary data-set-merging-summary-manual"
    else if (contains.fuzzy.match)
        "summary data-set-merging-summary data-set-merging-summary-fuzzy"
    else
        "summary data-set-merging-summary data-set-merging-summary-highlight"

    name.and.label <- variableNameAndLabelText(var.name, var.label)

    v.index.text <- variableIndexText(variable.index, num.span.width)

    indicators <- dataSetIndicators(input.var.names == "-")

    paste0("<summary class=\"", summary.classes, "\">", v.index.text,
           indicators, name.and.label, "</summary>")
}

variableIndexText <- function(variable.index, num.span.width)
{
    paste0("<span class=\"data-set-widget-var-num\" style=\"width:",
           num.span.width, "px\">", variable.index, ".</span>")
}

# Filled and unfilled squares used to indicate if a variable is present in the
# merged data set
dataSetIndicators <- function(indicator.states)
{
    indicators <- vapply(seq_along(indicator.states), function(j) {
            if (indicator.states[j])
                paste0("<span class=\"data-set-merging-indicator data-set-merging-indicator-fill\" title=\"Data set ",
                       j, "\">&#8193;</span>")
            else
                paste0("<span class=\"data-set-merging-indicator\" title=\"Data set ",
                       j, "\">&#8193;</span>")
        }, character(1))

    paste0("<span class=\"data-set-merging-indicator-container\">",
           paste0(indicators, collapse = ""), "</span>")
}

variableNameAndLabelText <- function(var.name, var.label)
{
    if (nchar(var.label) > 0)
    {
        if (substr(var.label, 1, nchar(var.name) + 1) == paste0(var.name, ":") ||
            var.name == var.label)
            htmlText(var.label)
        else
            paste0(htmlText(var.name), ": ", htmlText(var.label))
    }
    else
        htmlText(var.name)
}

inputVariableTable <- function(var.name, var.label, var.type, input.var.names,
                               input.var.labels, input.var.types,
                               is.fuzzy.input.var, input.var.matched.by,
                               n.data.sets)
{
    result <- paste0("<table class=\"data-set-merging-table data-set-merging-variable-table\"><thead>",
                     "<th>Data set</th><th>Variable name</th>",
                     "<th>Variable label</th><th>Variable type</th>",
                     "<th>Matched by</th></thead><tbody>")

    result <- paste0(result,
                     "<tr><td>Merged data set",
                     "</td><td>", htmlText(var.name),
                     "</td><td>", htmlText(var.label),
                     "</td><td>", htmlText(var.type),
                     "</td><td></td></tr>")

    is.summary.highlighted <- FALSE
    for (j in seq_len(n.data.sets))
    {
        is.manual <- !is.na(input.var.matched.by[j]) &&
                     input.var.matched.by[j] == "Manual"

        name.cell.class <- ""
        if (is.manual)
        {
            name.cell.class <- "data-set-merging-cell-manual"
            is.summary.highlighted <- TRUE
        }
        else if (is.fuzzy.input.var[j] &&
                 input.var.matched.by[j] == "Variable name")
        {
            name.cell.class <- "data-set-merging-cell-fuzzy"
            is.summary.highlighted <- TRUE
        }
        else if (input.var.names[j] != "-" &&
                 input.var.names[j] != var.name)
        {
            name.cell.class <- "data-set-merging-cell-highlight"
            is.summary.highlighted <- TRUE
        }

        label.cell.class <- ""
        if (is.fuzzy.input.var[j] &&
            input.var.matched.by[j] == "Variable label")
        {
            label.cell.class <- "data-set-merging-cell-fuzzy"
            is.summary.highlighted <- TRUE
        }
        else if (input.var.labels[j] != "-" &&
                 input.var.labels[j] != var.label)
        {
            label.cell.class <- "data-set-merging-cell-highlight"
            is.summary.highlighted <- TRUE
        }

        type.cell.class <- ""
        if (input.var.types[j] != "-" &&
            input.var.types[j] != var.type)
        {
            is.summary.highlighted <- TRUE
            type.cell.class <- "data-set-merging-cell-highlight"
        }

        matched.by <- "-"
        matched.by.class <- ""
        if (!is.na(input.var.matched.by[j]))
        {
            if (is.manual)
            {
                matched.by <- "Variable name (manual match)"
                matched.by.class <- "data-set-merging-cell-manual"
            }
            else if (is.fuzzy.input.var[j])
            {
                matched.by <- paste0(input.var.matched.by[j], " (fuzzy match)")
                matched.by.class <- "data-set-merging-cell-fuzzy"
            }
            else
                matched.by <- paste0(input.var.matched.by[j], " (exact match)")
        }

        result <- paste0(result,
                         "<tr><td>Input data set ", j,
                         "</td><td class=\"", name.cell.class,"\">",
                         htmlText(input.var.names[j]),
                         "</td><td class=\"", label.cell.class,"\">",
                         htmlText(input.var.labels[j]),
                         "</td><td class=\"", type.cell.class,"\">",
                         htmlText(input.var.types[j]),
                         "</td><td class=\"", matched.by.class,"\">",
                         matched.by,
                         "</td></tr>")
    }

    result <- paste0(result, "</tbody></table>")
    result <- gsub(" class=\"\"", "", result, fixed = TRUE) # do this to reduce size of widget
    attr(result, "is.summary.highlighted") <- is.summary.highlighted
    result
}

valueAttributesTable <- function(merged.val.attr, input.data.sets.metadata,
                                 input.val.attr.list, input.var.ind)
{
    cat.types <- c("Categorical", "Categorical with string values")
    input.var.types <- input.data.sets.metadata$variable.types
    input.var.val.attr <- input.data.sets.metadata$variable.value.attributes
    n.data.sets <- input.data.sets.metadata$n.data.sets

    result <- paste0("<table class=\"data-set-merging-table data-set-merging-val-attr-table\">",
                     "<thead><th colspan=2 style=\"text-align:center\">Merged data set</th>",
                     paste0(paste0("<th>Input data set ", seq_len(n.data.sets),
                                   "</th>"), collapse = ""), "</thead><tbody>")

    is.summary.highlighted <- FALSE
    for (j in seq_len(length(merged.val.attr)))
    {
        result <- paste0(result, "<tr><td>",
                         htmlText(names(merged.val.attr)[j]),
                         "</td><td>", htmlText(merged.val.attr[j]), "</td>")

        for (k in seq_len(n.data.sets))
        {
            result <- if (!is.na(input.val.attr.list[[k]][j]))
            {
                val <- input.val.attr.list[[k]][j]

                if (input.var.types[[k]][input.var.ind[k]] %in% cat.types)
                {
                    val.attr <- input.var.val.attr[[k]][[input.var.ind[k]]]
                    lbl <- names(val.attr)[val.attr == input.val.attr.list[[k]][j]]
                    cell.class <- if (input.val.attr.list[[k]][j] != merged.val.attr[j] ||
                                      lbl != names(merged.val.attr)[j])
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
                           tolower(input.var.types[[k]][input.var.ind[k]]),
                           " variable\" class=\"data-set-merging-cell-highlight\">",
                           htmlText(val), "</td>")
                }
            }
            else # Missing value
                paste0(result, "<td>-</td>")
        }
        result <- paste0(result, "</tr>")
    }
    result <- paste0(result, "</tbody></table>")
    result <- gsub(" class=\"\"", "", result, fixed = TRUE) # do this to reduce size of widget
    attr(result, "is.summary.highlighted") <- is.summary.highlighted
    result
}

mergingNote <- function(omitted.variable.names)
{
    n.omitted <- vapply(omitted.variable.names, length, integer(1))

    html <- ""
    if (any(n.omitted > 0))
    {
        html <- paste0(html, "<div class=\"data-set-widget-title\">",
                       "Note:", "</div>")

        for (i in which(n.omitted > 0))
        {
            omitted <- omitted.variable.names[[i]]
            html <- paste0(html, "<div class=\"data-set-widget-note\">",
                           "The following variable",
                           ngettext(length(omitted), "", "s"),
                           " from data set ", i,
                           ngettext(length(omitted), " was", " were"),
                           " omitted: ",
                           paste0("<b>", htmlText(omitted), "</b>",
                                  collapse = ", "), ".</div>")
        }
    }
    html
}