#' @title Widget Output for Data Set Merging By Variable
#' @description Widget shown in output for \code{flipData::MergeDataSetsByVariable}.
#' @param input.data.sets.metadata An object containing metadata for the
#'   input data set.
#' @param merged.data.set.metadata An object containing metadata for the
#'   merged data set.
#' @param omitted.variables A list where each element contains the names of the
#'   variables omitted from a data set.
#' @param is.saved.to.cloud Whether the merged data set was saved to the
#'   Displayr cloud drive.
#' @export
DataSetMergingByVariableWidget <- function(input.data.sets.metadata,
                                           merged.data.set.metadata,
                                           omitted.variables,
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
                                                   is.saved.to.cloud))

    n.vars <- merged.data.set.metadata$n.variables
    num.span.width <- ceiling(log10(n.vars + 1)) * 10 + 15
    html.vars <- rep(NA_character_, n.vars)

    for (i in seq_len(n.vars))
    {
        var.name <- merged.data.set.metadata$variable.names[i]
        var.label <- merged.data.set.metadata$variable.labels[i]
        v.index.text <- variableIndexText(i, num.span.width)
        name.and.label <- variableNameAndLabelText(var.name, var.label)
        html.vars[i] <- paste0("<div class=\"data-set-widget-row\">",
                               v.index.text, name.and.label, "</div>")
        # rows of variables, indicating index, data set index, name, label
        # highlight ID variable, which expands to show input ID variables, example value
    }

    cata(paste0(html,
                paste0(html.vars, collapse = ""),
                mergingNote(omitted.variables),
                "</div>")) # close data-set-merging-main-container

    createWidgetFromFile(tfile)
}

mergingByVariableSubtitle <- function(merged.data.set.metadata,
                                      is.saved.to.cloud)
{
    html <- ""

    if (is.saved.to.cloud)
        html <- paste0(html, "<div class=\"data-set-widget-subtitle\">(saved to Displayr cloud drive)</div>")

    html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                   merged.data.set.metadata$n.variables, " variables, ",
                   merged.data.set.metadata$n.cases, " cases</div>")

    # legend for ID variable

    html
}
