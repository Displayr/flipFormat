#' @title Stacking Output Widget
#' @description Stacking widget shown in output for \code{flipData::StackData}.
#' @param input.data.set.metadata An object containing metadata for the
#'  input data set.
#' @param stacked.data.set.metadata An object containing metadata for the
#'   stacked data set.
#' @param unstackable.names A list of character vectors of the names of
#'   variables that could not be stacked due to mismatching types or
#'   value attributes.
#' @param common.labels.list A list of sets of common labels used for stacking.
#' @param is.saved.to.cloud Whether the stacked data set was saved to the
#'   Displayr cloud drive.
#' @export
StackingWidget <- function(input.data.set.metadata,
                           stacked.data.set.metadata,
                           unstackable.names,
                           common.labels.list,
                           is.saved.to.cloud)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("datasetwidget.css", cata)
    addCss("stacking.css", cata)

    html <- "<div class=\"data-set-widget-main-container\">"
    html <- paste0(html, stackingTitleAndSubTitle(stacked.data.set.metadata,
                                                  common.labels.list,
                                                  is.saved.to.cloud))
    html <- paste0(html, stackedDataSetOutput(stacked.data.set.metadata))
    html <- paste0(html, noteOutput(unstackable.names))
    html <- paste0(html, inputDataSetOutput(input.data.set.metadata,
                                            stacked.data.set.metadata))
    html <- paste0(html, "</div>")

    cata(html)

    createWidgetFromFile(tfile)
}

stackingTitleAndSubTitle <- function(stacked.data.set.metadata,
                                     common.labels.list, is.saved.to.cloud)
{
    md <- stacked.data.set.metadata

    if (length(md$variable.names) == 0)
        return(paste0("<div class=\"data-set-widget-title\">",
                       "No stacking was conducted</div>"))

    html <- paste0("<div class=\"data-set-widget-title\">",
                   htmlText(md$data.set.name), "</div>")
    if (is.saved.to.cloud)
        html <- paste0(html, "<div class=\"data-set-widget-subtitle\">(saved to Displayr cloud drive)</div>")

    n.stacked.vars <- sum(md$is.stacked.variable)
    n.manually.stacked.vars <- sum(md$is.manually.stacked.variable, na.rm = TRUE)
    n.common.lbl.stacked.vars <- n.stacked.vars - n.manually.stacked.vars

    html <- paste0(html, "<div class=\"data-set-widget-subtitle\">", md$n.variables,
                   " variables, ",  n.stacked.vars, " stacked variables (",
                   n.common.lbl.stacked.vars, " stacked from common labels, ",
                   n.manually.stacked.vars, " manually stacked)</div>")

    if (!is.null(common.labels.list))
    {
        if (length(common.labels.list) == 1)
            html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                           length(common.labels.list[[1]]), " common labels: ",
                           paste0(htmlText(common.labels.list[[1]]), collapse = ", "),
                           "</div>")
        else
            for (i in seq_along(common.labels.list))
            {
                common.labels <- common.labels.list[[i]]
                html <- paste0(html, "<div class=\"data-set-widget-subtitle\">",
                               length(common.labels), " common labels (set ", i, "): ",
                               paste0(htmlText(common.labels), collapse = ", "),
                               "</div>")
            }
    }
    html
}

stackedDataSetOutput <- function(stacked.data.set.metadata)
{
    md <- stacked.data.set.metadata

    if (length(md$variable.names) == 0)
        return("")

    num.span.width <- ceiling(log10(md$n.variables + 1)) * 10 + 15

    ind <- match(FALSE, vapply(md$stacking.input.variable.names, is.null, logical(1)))
    proportion.span.width <- if (!is.na(ind))
    {
        n.stacking <- length(md$stacking.input.variable.names[[ind]])
        ceiling(log10(n.stacking + 1)) * 10 * 2 + 20
    }
    else
        0

    output.var.limit <- 10000
    n.variables.to.show <- min(md$n.variables, output.var.limit)
    if (md$n.variables > output.var.limit)
        warning("Due to the large number of variables in the output data set (",
                md$n.variables, "), only the first ", output.var.limit,
                " variables have been shown.")

    html.rows <- character(n.variables.to.show)
    for (i in seq_len(n.variables.to.show))
    {
        row.title <- variableNameAndLabelText(md$variable.names[i],
                                              md$variable.labels[i])

        if (!md$is.stacked.variable[i])
        {
            html.row <- paste0("<div class=\"data-set-widget-row\">",
                               "<span class=\"data-set-widget-var-num\" style=\"width:",
                               num.span.width + proportion.span.width, "px\">",
                               i, ".</span>", row.title, "</div>")
        }
        else
        {
            table.html <- stackingTable(md, i)

            summary.class <- if (md$is.manually.stacked[i])
                "stacking-summary stacking-summary-manual"
            else
                "stacking-summary"

            description <- if (md$is.manually.stacked[i])
                "<div class=\"stacking-description\">Manually stacked:</div>"
            else
                "<div class=\"stacking-description\">Stacked using common labels:</div>"

            prop.text <- paste0("(", sum(!is.na(md$stacking.input.variable.names[[i]])), "/",
                                length(md$stacking.input.variable.names[[i]]), ")")

            html.row <- paste0("<details class=\"stacking-details\">",
                               "<summary class=\"", summary.class, "\">",
                               "<span class=\"data-set-widget-var-num\" style=\"width:",
                               num.span.width, "px\">", i, ".</span>",
                               "<span class=\"stacking-proportion\" style=\"width:",
                               proportion.span.width, "px\" ",
                               "title=\"Proportion of non-missing observations\">",
                               prop.text, "</span>",
                               htmlText(row.title),
                               "</summary>", description,
                               table.html, "</table></details>")
        }
        html.rows[i] <- html.row
    }
    paste0(html.rows, collapse = "")
}

stackingTable <- function(stacked.data.set.metadata, var.ind)
{
    md <- stacked.data.set.metadata

    table.html <- paste0("<table class=\"stacking-table\"><thead>",
                         "<th></th><th>Name</th><th>Label</th></thead>",
                         "<tbody><tr><td>Stacked</td><td>",
                         htmlText(md$variable.names[var.ind]), "</td><td>",
                         htmlText(md$variable.labels[var.ind]), "</td></tr>")

    stacking.input.var.names <- md$stacking.input.variable.names[[var.ind]]
    stacking.input.var.labels <- md$stacking.input.variable.labels[[var.ind]]

    table.html <- paste0(table.html,
                         paste0(vapply(seq_along(stacking.input.var.names), function(j) {
                             if (is.na(stacking.input.var.names[j]))
                                 paste0("<tr><td>Observation ", j, "</td><td></td><td></td></tr>")
                             else
                                 paste0("<tr><td>Observation ", j, "</td><td>",
                                        htmlText(stacking.input.var.names[j]), "</td><td>",
                                        htmlText(stacking.input.var.labels[j]), "</td></tr>")
                         }, character(1)), collapse = ""))

    paste0(table.html, "</tbody></table></details>")
}

inputDataSetOutput <- function(input.data.set.metadata,
                               stacked.data.set.metadata)
{
    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels
    n.variables <- input.data.set.metadata$n.variables
    input.var.limit <- 10000
    n.variables.to.show <- min(n.variables, input.var.limit)
    if (n.variables > input.var.limit)
        warning("Due to the large number of variables in the input data set (",
                md$n.variables, "), only the first ", input.var.limit,
                " variables have been shown.")

    num.span.width <- ceiling(log10(n.variables.to.show + 1)) * 10 + 15

    html.rows <- character(n.variables.to.show)
    for (i in seq_len(n.variables.to.show))
    {
        row.title <- variableNameAndLabelText(v.names[i],
                                              v.labels[i])
        html.rows[i] <- paste0("<div class=\"data-set-widget-row\">",
                               "<span class=\"data-set-widget-var-num\" style=\"width:",
                               num.span.width, "px\">",
                               i, ".</span>", row.title, "</div>")
    }

    paste0("<details open=\"true\" class=\"stacking-input-details\">",
           "<summary class=\"stacking-input-summary\">",
           "<span class=\"data-set-widget-title\">",
           "Input data set: ", htmlText(input.data.set.metadata$data.set.name),
           "</span></summary>", paste0(html.rows, collapse = ""), "</details>")
}

noteOutput <- function(unstackable.names)
{
    if(length(unstackable.names) == 0)
        return("")

    html <- paste0("<div class=\"stacking-note-container\">",
                   "<div class=\"data-set-widget-title\">",
                   "Note:", "</div>")

    html <- paste0(html, paste0(vapply(unstackable.names, function(nms) {
        paste0("<div class=\"stacking-note\">The following variables could not be ",
               "stacked using common labels due to mismatching variable types or value attributes: ",
               paste0("<b>", nms, "</b>", collapse = ", "), ".</div>")
    }, character(1)), collapse = ""))

    paste0(html, "</div>")
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
