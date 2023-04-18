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
#' @param page A numeric scalar of the page number. If not specified, the first
#'   page is shown.
#' @param variables.per.page A numeric scalar of the number of variables per
#'   page.
#' @export
DataSetMergingByVariableWidget <- function(input.data.sets.metadata,
                                           merged.data.set.metadata,
                                           source.data.set.indices,
                                           omitted.variable.names.list,
                                           merged.id.variable.name,
                                           id.variable.names,
                                           example.id.values,
                                           is.saved.to.cloud,
                                           page = NULL,
                                           variables.per.page = 1000)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("datasetwidget.css", cata)
    addCss("datasetmerging.css", cata)

    title.html <- paste0("<div class=\"data-set-widget-title\">",
                         htmlText(merged.data.set.metadata$data.set.name),
                         "</div>")

    n.vars <- merged.data.set.metadata$n.variables
    page.var.ind <- variableIndicesInPage(n.vars, page, variables.per.page)

    subtitle.html <-  mergingByVariableSubtitle(merged.data.set.metadata,
                                                is.saved.to.cloud,
                                                !is.null(merged.id.variable.name),
                                                n.vars, page,
                                                variables.per.page,
                                                page.var.ind)

    num.span.width <- variableIndexSpanWidth(page.var.ind)
    html.vars <- rep(NA_character_, length(page.var.ind))
    n.data.sets <- input.data.sets.metadata$n.data.sets

    k <- 1
    for (var.ind in page.var.ind)
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
            html.vars[k] <- paste0("<details class=\"details data-set-merging-details\">",
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
            html.vars[k] <- paste0("<div class=\"data-set-widget-row\">",
                                         v.index.text, indicators, name.and.label,
                                         "</div>")
        }
        k <- k + 1
    }

    note.html <- mergingNote(omitted.variable.names.list, page, n.vars,
                             variables.per.page)

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
#' @param n.vars A numeric scalar of the number of variables in the merged data
#'  set
#' @param page A numeric scalar of the page number. If not specified, the first
#'   page is shown.
#' @param variables.per.page A numeric scalar of the number of variables per
#'   page.
#' @param page.var.ind A numeric vector of the variable indices in the page.
#' @return Character scalar containing the HTML for the subtitle.
#' @noRd
mergingByVariableSubtitle <- function(merged.data.set.metadata,
                                      is.saved.to.cloud, has.id.variable,
                                      n.vars, page, variables.per.page,
                                      page.var.ind)
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

    if (!is.null(page) && n.vars > variables.per.page) {
        page.subtitle <- pageSubtitle(n.vars, page, variables.per.page,
                                      page.var.ind)
        html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                       page.subtitle, "</div>")
    }

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
#' @param page A numeric scalar of the page number. If not specified, the first
#'   page is shown.
#' @param n.vars A numeric scalar of the number of variables in the merged data
#'  set
#' @param variables.per.page A numeric scalar of the number of variables per
#'   page.
#' @return Character scalar containing the HTML for the notes that are shown at
#'  the bottom of the output. Currently only shows the omitted variables from
#'  each data set.
#' @noRd
mergingNote <- function(omitted.variable.names.list, page, n.vars,
                        variables.per.page)
{
    has.truncation.warning <- is.na(page) && n.vars > variables.per.page
    n.omitted <- vapply(omitted.variable.names.list, length, integer(1))

    html <- ""
    if (has.truncation.warning || any(n.omitted > 0))
    {
        html <- paste0(html, "<div class=\"data-set-widget-title\">",
                       "Note:", "</div>")

        if (has.truncation.warning) {
            html <- paste0(html, "<div class=\"data-set-widget-subtitle-warning\">",
                           "Only the first ",
                           variables.per.page, " variables are shown. ",
                           'Click on "View Variables from Merged Data" in the Inputs tab to view the other variables',
                           "</div>")
        }

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
