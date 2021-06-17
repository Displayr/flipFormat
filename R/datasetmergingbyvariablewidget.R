#' @title Widget Output for Data Set Merging By Variable
#' @description Widget shown in output for \code{flipData::MergeDataSetsByVariable}.
#' @param input.data.sets.metadata An object containing metadata for the
#'   input data set.
#' @param merged.data.set.metadata An object containing metadata for the
#'   merged data set.
#' @param omitted.variable.names A list where each element contains the names
#'   of the variables omitted from a data set.
#' @param is.saved.to.cloud Whether the merged data set was saved to the
#'   Displayr cloud drive.
#' @export
DataSetMergingByVariableWidget <- function(input.data.sets.metadata,
                                           merged.data.set.metadata,
                                           source.data.set.indices,
                                           omitted.variable.names,
                                           merged.id.variable.name,
                                           id.variable.names,
                                           example.id.values,
                                           is.saved.to.cloud)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("datasetwidget.css", cata)
    addCss("datasetmerging.css", cata)

    html <- paste0("<div class=\"data-set-widget-main-container\">",
                   "<div class=\"data-set-widget-title\">",
                   htmlText(merged.data.set.metadata$data.set.name),
                   "</div>")

    html <- paste0(html, mergingByVariableSubtitle(merged.data.set.metadata,
                                                   is.saved.to.cloud,
                                                   !is.null(merged.id.variable.name)))

    n.vars <- merged.data.set.metadata$n.variables
    num.span.width <- ceiling(log10(n.vars + 1)) * 10 + 15
    html.vars <- rep(NA_character_, n.vars)
    n.data.sets <- input.data.sets.metadata$n.data.sets

    for (i in seq_len(n.vars))
    {
        var.name <- merged.data.set.metadata$variable.names[i]
        var.label <- merged.data.set.metadata$variable.labels[i]
        v.index.text <- variableIndexText(i, num.span.width)
        name.and.label <- variableNameAndLabelText(var.name, var.label)

        if (var.name == merged.id.variable.name)
        {
            id.var.table <- idVariableTable(id.variable.names,
                                            example.id.values,
                                            input.data.sets.metadata)
            indicators <- dataSetIndicators(rep(TRUE, n.data.sets))
            html.vars[i] <- paste0("<details class=\"details data-set-merging-details\">",
                                   "<summary class=\"data-set-merging-summary data-set-merging-summary-id\">",
                                   v.index.text, indicators,
                                   name.and.label, "</summary>", id.var.table,
                                   "</details>")
        }
        else
        {
            indicator.states <- rep(FALSE, n.data.sets)
            indicator.states[source.data.set.indices[i]] <- TRUE
            indicators <- dataSetIndicators(indicator.states)
            html.vars[i] <- paste0("<div class=\"data-set-widget-row\">",
                                   v.index.text, indicators, name.and.label,
                                   "</div>")
        }
    }

    cata(paste0(html,
                paste0(html.vars, collapse = ""),
                mergingNote(omitted.variable.names),
                "</div>")) # close data-set-merging-main-container

    createWidgetFromFile(tfile)
}

mergingByVariableSubtitle <- function(merged.data.set.metadata,
                                      is.saved.to.cloud, has.id.variable)
{
    html <- ""

    if (is.saved.to.cloud)
        html <- paste0(html, "<div class=\"data-set-widget-subtitle\">(saved to Displayr cloud drive)</div>")

    html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                   merged.data.set.metadata$n.variables, " variables, ",
                   merged.data.set.metadata$n.cases, " cases</div>")

    if (has.id.variable)
        html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                       "Legend: <span class=\"data-set-widget-id-shade\">",
                       "&nbsp;ID&nbsp;variable&nbsp;</span></div>")

    html
}

idVariableTable <- function(id.variable.names,
                            example.id.values,
                            input.data.sets.metadata)
{
    v.names <- input.data.sets.metadata$variable.names
    v.labels <- input.data.sets.metadata$variable.labels

    result <- paste0("<table class=\"data-set-merging-table\"><thead>",
                     "<th>Data set</th><th>ID variable name</th>",
                     "<th>ID variable label</th><th>Example value</th>",
                     "</thead><tbody>")
    for (i in seq_along(id.variable.names))
    {
        nm <- id.variable.names[i]
        lbl <- v.labels[[i]][nm == v.names[[i]]]
        result <- paste0(result, "<tr><td>", i, "</td><td>",
                         nm, "</td><td>", lbl, "</td><td>",
                         example.id.values[i], "</td></tr>")
    }
    result <- paste0(result, "</tbody></table>")

    result
}
