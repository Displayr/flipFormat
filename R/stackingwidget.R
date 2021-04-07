#' @title Stacking Output Widget
#' @description Stacking widget shown in output for \code{flipData::StackData}.
#' @param stacked.data.set.metadata An object containing metadata for the
#'   stacked data set.
#' @param unstackable.names A list of character vectors of the names of
#'   variables that could not be stacked due to mismatching types or
#'   value attributes.
#' @param omitted.variables A character vector of omitted variables.
#' @param omitted.stacked.variables A character vector of omitted stacked
#'   variables.
#' @param common.labels The common labels used for stacking.
#' @param is.saved.to.cloud Whether the stacked data set was saved to the
#'   Displayr cloud drive.
#' @export
StackingWidget <- function(stacked.data.set.metadata,
                           unstackable.names,
                           omitted.variables,
                           omitted.stacked.variables,
                           common.labels,
                           is.saved.to.cloud)
{
    md <- stacked.data.set.metadata

    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("stacking.css", cata)

    html <- paste0("<div class=\"stacking-main-container\">",
                   "<div class=\"stacking-title\">",
                   htmlText(md$data.set.name), "</div>")
    if (is.saved.to.cloud)
        html <- paste0(html, "<div class=\"stacking-subtitle\">(saved to Displayr cloud drive)</div>")

    html <- paste0(html, "<div class=\"stacking-subtitle\">", md$n.variables,
                   " variables, ",
                   sum(md$is.stacked.variable) - sum(md$is.manually.stacked.variable, na.rm = TRUE),
                   " stacked variables from common labels, ",
                   sum(md$is.manually.stacked.variable, na.rm = TRUE),
                   " manually stacked variables</div>")

    if (!is.null(common.labels))
        html <- paste0(html, "<div class=\"stacking-subtitle\">",
                       "Common labels: ", paste0(htmlText(common.labels), collapse = ", "),
                       "</div>")

    num.span.width <- ceiling(log10(md$n.variables + 1)) * 10 + 15

    html.rows <- character(md$n.variables)
    for (i in seq_len(md$n.variables))
    {
        row.title <- paste0(md$variable.names[i], ": ",
                            md$variable.labels[i])
        if (!(md$is.stacked.variable[i]))
        {
            html.row <- paste0("<div class=\"stacking-row\">",
                               "<span class=\"stacking-var-num\" style=\"width:",
                               num.span.width, "px\">", i, ".</span>",
                               htmlText(row.title), "</div>")
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

            html.row <- paste0("<details class=\"stacking-details\">",
                               "<summary class=\"", summary.class, "\">",
                               "<span class=\"stacking-var-num\" style=\"width:",
                               num.span.width, "px\">", i, ".</span>",
                               htmlText(row.title),
                               "</summary>", description,
                               table.html, "</table></details>")
        }
        html.rows[i] <- html.row
    }
    html <- paste0(html, paste0(html.rows, collapse = ""))

    # Whether to show Note
    if (length(unstackable.names) > 0 ||
        length(omitted.variables) > 0)
    {
        html <- paste0(html, "<div class=\"stacking-note-container\">",
                       "<div class=\"stacking-title\">",
                       "Note:", "</div>")

        html <- paste0(html, paste0(vapply(unstackable.names, function(nms) {
            paste0("<div class=\"stacking-note\">The following variables could not be ",
                   "stacked using common labels due to mismatching variable types or value attributes: ",
                   paste0("<b>", nms, "</b>", collapse = ", "), ".</div>")
        }, character(1)), collapse = ""))


        if (length(omitted.stacked.variables) > 0)
            html <- paste0(html, "<div class=\"stacking-note\">The following <b>stacked</b> variable",
                           ngettext(length(omitted.stacked.variables), " has", "s have"),
                           " been omitted from the stacked data set: ",
                           paste0("<b>", omitted.stacked.variables, "</b>", collapse = ", "),
                           ".</div>")

        omitted.non.stacked.variables <- setdiff(omitted.variables,
                                                 omitted.stacked.variables)

        if (length(omitted.non.stacked.variables) > 0)
            html <- paste0(html, "<div class=\"stacking-note\">The following variable",
                           ngettext(length(omitted.non.stacked.variables), " has", "s have"),
                           " been omitted from the stacked data set: ",
                           paste0("<b>", omitted.non.stacked.variables, "</b>", collapse = ", "),
                           ".</div>")
        html <- paste0(html, "</div>")
    }
    html <- paste0(html, "</div>")

    cata(html)

    createWidgetFromFile(tfile)
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



TestWidget2 <- function(widget, name, delay = 0.2, threshold = 0.001, ...,
          diff.path = "snapshots/diff", accepted.path = "snapshots/accepted")
{
    if (!dir.exists(diff.path))
        stop("Directory ", diff.path, " does not exist.")
    if (!dir.exists(accepted.path))
        stop("Directory ", accepted.path, " does not exist.")
    diff.file <- paste0(diff.path, "/", name, ".png")
    accepted.file <- paste0(accepted.path, "/", name, ".png")
    suppressWarnings(CompareSnapshot2(widget, diff.file, accepted.file,
                                     delay, threshold, FALSE, ...))
}

#' @importFrom visualTest isSimilar
CompareSnapshot2 <- function(widget, diff.file, accepted.file, delay = 0.2, threshold = 0.1,
          strict = FALSE, ...)
{
    if (!file.exists(accepted.file)) {
        CreateSnapshot2(widget, filename = accepted.file, delay = delay,
                       ...)
        if (strict)
            warning("File ", accepted.file, " does not exist.")
        return(TRUE)
    }
    else {
        CreateSnapshot2(widget, filename = diff.file, delay = delay,
                       ...)
        # res <- isSimilar(file = diff.file, fingerprint = accepted.file,
        #                  threshold = threshold)
        # if (res)
        #     unlink(diff.file)
        # return(res)
        return(TRUE)
    }
}

#' @importFrom htmlwidgets saveWidget
#' @import chromote
CreateSnapshot2  <- function(widget, filename, delay = 0, width = 992, height = 744,
         mouse.hover = TRUE, mouse.click = FALSE, mouse.doubleclick = FALSE,
         mouse.xpos = 0.5, mouse.ypos = 0.5)
{
    if (inherits(widget, "StandardChart"))
        widget <- widget$htmlwidget
    if (!grepl(".png$", tolower(filename)))
        filename <- paste0(filename, ".png")
    tmp.files <- tempdir()
    tmp.html <- paste0(tmp.files, ".html")
    on.exit(unlink(tmp.html), add = TRUE)
    on.exit(unlink(tmp.files, recursive = TRUE), add = TRUE)
    saveWidget(widget, file = tmp.html, selfcontained = FALSE)
    # b <- ChromoteSession$new(width = width, height = height)
    # b$Page$navigate(paste0("file://", tmp.html))
    # xpos <- mouse.xpos * width
    # ypos <- mouse.ypos * height
    # if (mouse.click || mouse.doubleclick) {
    #     b$Page$loadEventFired()
    #     b$Input$dispatchMouseEvent(type = "mousePressed",
    #                                x = xpos, y = ypos, button = "left", pointerType = "mouse",
    #                                clickCount = if (mouse.doubleclick)
    #                                    2
    #                                else 1)
    #     b$Input$dispatchMouseEvent(type = "mouseReleased",
    #                                x = xpos, y = ypos, button = "left", pointerType = "mouse",
    #                                clickCount = if (mouse.doubleclick)
    #                                    2
    #                                else 1)
    # }
    # else if (mouse.hover)
    #     b$Input$dispatchMouseEvent(type = "mouseMoved",
    #                                x = xpos, y = ypos)
    # if (isTRUE(is.finite(delay)) && delay > 0)
    #     Sys.sleep(delay)
    # b$screenshot(filename)
    # invisible(b$close())
}

