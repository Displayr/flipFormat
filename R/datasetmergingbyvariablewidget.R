#' @title Widget Output for Data Set Merging By Variable
#' @description Widget shown in output for \code{flipData::MergeDataSetsByVariable}.
#' @param input.data.sets.metadata An object containing metadata for the
#'   input data set. See the function \code{metadataFromDataSets} in flipData
#'   for more information.
#' @param merged.data.set.metadata An object containing metadata for the
#'   merged data set. See the function \code{metadataFromDataSet} in flipData
#'   for more information.
#' @param source.data.set.indices An integer vector corresponding to the
#'   variables in the merged data set. Each element contains the index of the
#'   input data set from which the variable originated.
#' @param omitted.variable.names.list A list whose elements correspond to the
#'   input data sets. Each element contains the names of variables from a data
#'   set that were omitted from the merged data set.
#' @param merged.id.variable.name A character scalar of the name of the ID
#'   variable in the merged data set. It is NULL if there is no ID variable.
#' @param id.variable.names A character vector corresponding to the input data
#'   sets. Each element is an ID variable name from an input data set.
#' @param example.id.values A character vector corresponding to the input data
#'   sets. Each element is an example ID value from an ID variable from an
#'   input data set.
#' @param is.saved.to.cloud A logical scalar indicating whether the merged data
#'   set was saved to the Displayr cloud drive.
#' @export
DataSetMergingByVariableWidget <- function(input.data.sets.metadata,
                                           merged.data.set.metadata,
                                           source.data.set.indices,
                                           omitted.variable.names.list,
                                           merged.id.variable.name,
                                           id.variable.names,
                                           example.id.values,
                                           is.saved.to.cloud)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("datasetwidget.css", cata)
    addCss("datasetmerging.css", cata)

    title.html <- paste0("<div class=\"data-set-widget-title\">",
                         htmlText(merged.data.set.metadata$data.set.name),
                         "</div>")

    subtitle.html <-  mergingByVariableSubtitle(merged.data.set.metadata,
                                                is.saved.to.cloud,
                                                !is.null(merged.id.variable.name))

    n.vars <- merged.data.set.metadata$n.variables
    output.var.limit <- 10000
    n.variables.to.show <- min(n.vars, output.var.limit)
    if (n.vars > output.var.limit)
        warning("Due to the large number of variables in the output data set (",
                n.vars, "), only the first ", output.var.limit,
                " variables have been shown.")


    num.span.width <- ceiling(log10(n.variables.to.show + 1)) * 10 + 15
    html.vars <- rep(NA_character_, n.variables.to.show)
    n.data.sets <- input.data.sets.metadata$n.data.sets

    for (var.ind in seq_len(n.variables.to.show))
    {
        var.name <- merged.data.set.metadata$variable.names[var.ind]
        var.label <- merged.data.set.metadata$variable.labels[var.ind]
        v.index.text <- variableIndexSpan(var.ind, num.span.width)
        name.and.label <- variableNameAndLabelText(var.name, var.label)

        if (!is.null(merged.id.variable.name) &&
            var.name == merged.id.variable.name) # ID variable
        {
            id.var.table <- idVariableTable(id.variable.names,
                                            example.id.values,
                                            input.data.sets.metadata)
            indicators <- dataSetIndicatorsSpan(rep(TRUE, n.data.sets))
            html.vars[var.ind] <- paste0("<details class=\"details data-set-merging-details\">",
                                         "<summary class=\"data-set-merging-summary data-set-merging-summary-id\">",
                                         v.index.text, indicators,
                                         name.and.label, "</summary>", id.var.table,
                                         "</details>")
        }
        else # Non-ID variable
        {
            indicator.states <- rep(FALSE, n.data.sets)
            indicator.states[source.data.set.indices[var.ind]] <- TRUE
            indicators <- dataSetIndicatorsSpan(indicator.states)
            html.vars[var.ind] <- paste0("<div class=\"data-set-widget-row\">",
                                         v.index.text, indicators, name.and.label,
                                         "</div>")
        }
    }

    note.html <- mergingNote(omitted.variable.names.list)

    cata(paste0("<div class=\"data-set-widget-main-container\">",
                title.html,
                subtitle.html,
                paste0(html.vars, collapse = ""),
                note.html,
                "</div>"))

    createWidgetFromFile(tfile)
}

#' @param merged.data.set.metadata See documentation for
#'  merged.data.set.metadata in DataSetMergingByVariableWidget.
#' @param is.saved.to.cloud See documentation for
#'  is.saved.to.cloud in DataSetMergingByVariableWidget.
#' @param has.id.variable Logical scalar indicating whether the merged data set
#'  contains an ID variable.
#' @return Character scalar containing the HTML for the subtitle.
#' @noRd
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

#' @param id.variable.names See documentation for
#'  id.variable.names in DataSetMergingByVariableWidget.
#' @param example.id.values See documentation for
#'  example.id.values in DataSetMergingByVariableWidget.
#' @param input.data.sets.metadata See documentation for
#'  input.data.sets.metadata in DataSetMergingByVariableWidget.
#' @return Character scalar containing the HTML for a table showing the data
#'  set index, name, label and example value of each ID variable used for
#'  matching cases.
#' @noRd
idVariableTable <- function(id.variable.names,
                            example.id.values,
                            input.data.sets.metadata)
{
    v.names.list <- input.data.sets.metadata$variable.names.list
    v.labels.list <- input.data.sets.metadata$variable.labels.list

    result <- paste0("<table class=\"data-set-merging-table data-set-merging-id-table\"><thead>",
                     "<th>Data set</th><th>ID variable name</th>",
                     "<th>ID variable label</th><th>Example value</th>",
                     "</thead><tbody>")
    for (i in seq_along(id.variable.names))
    {
        nm <- id.variable.names[i]
        lbl <- v.labels.list[[i]][nm == v.names.list[[i]]]
        result <- paste0(result, "<tr><td>", i, "</td><td>",
                         nm, "</td><td>", lbl, "</td><td>",
                         example.id.values[i], "</td></tr>")
    }
    result <- paste0(result, "</tbody></table>")

    result
}

#' @param omitted.variable.names.list See documentation for
#'  omitted.variable.names.list in DataSetMergingByVariableWidget.
#' @return Character scalar containing the HTML for the notes that are shown at
#'  the bottom of the output. Currently only shows the omitted variables from
#'  each data set.
#' @noRd
mergingNote <- function(omitted.variable.names.list)
{
    n.omitted <- vapply(omitted.variable.names.list, length, integer(1))

    html <- ""
    if (any(n.omitted > 0))
    {
        html <- paste0(html, "<div class=\"data-set-widget-title\">",
                       "Note:", "</div>")

        for (i in which(n.omitted > 0))
        {
            omitted <- omitted.variable.names.list[[i]]
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

#' @param variable.index Integer scalar of the variable index to be shown.
#' @param num.span.width Integer scalar of the width of the span in pixels.
#' @return Character scalar containing a HTML span showing the variable index
#'  with a predefined width.
#' @noRd
variableIndexSpan <- function(variable.index, num.span.width)
{
    paste0("<span class=\"data-set-widget-var-num\" style=\"width:",
           num.span.width, "px\">", variable.index, ".</span>")
}

#' @param indicator.states Logical vector representing indicator states.
#' @return Character scalar containing the HTML span which shows data set
#'  indicators: filled and unfilled squares indicating whether a variable
#'  contains input from an input data set.
#' @noRd
dataSetIndicatorsSpan <- function(indicator.states)
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

#' @param var.name Character scalar of the variable name to display.
#' @param var.label Character scalar of the variable label to display.
#' @return Character scalar showing the variable name and label separated by a
#'  colon. If the name and label are the same, only show one of them.
#' @noRd
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
