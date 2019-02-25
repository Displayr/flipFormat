#' Display text analysis output as an \code{htmlwidget}
#'
#' Creates a \code{htmlwidget} summary of diagnostic information for a
#' text analysis output from flipTextAnalysis.
#' @param raw.and.normalized.text A data frame with two variables, the first
#'     being the raw text and the second being the normalized text. The row
#'     names of the data frame should correspond to the case index.
#' @param n.gram.frequencies A data frame with two variables, the first being
#'     the n-gram and the second being the frequencies.
#' @param footer Character; footer to show at the bottom of the output.
#' @return An \code{htmlwidget} containing diagnostic information for
#'     the experimental design, including D-error, standard errors,
#'     frequenices, pairwise frequencies, the labeled design, and
#'     prior information. An attribute called \code{"ChartData"} also
#'     contains the labeled design.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
CreateTextAnalysisWidget <- function(raw.and.normalized.text,
                                     n.gram.frequencies, footer)
{
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("table.css", cata)
    addCss("textanalysis.css", cata)

    cata("<div class=\"main-container\">")

    addLeftPanel(raw.and.normalized.text, cata)
    addRightPanel(n.gram.frequencies, cata)

    cata("<div id=\"footer-container\">")
    cata(paste0("<p id=\"footer\">", footer,"</p>"))
    cata("</div>", fill = TRUE) # end footer-container div

    cata("</div>", fill = TRUE) # end main-container div

    createWidgetFromFile(tfile)
}

# refactor code in CreateChoiceModelDesignWidget
createTempFile <- function()
{
    tfile <- tempfile(fileext = ".html")
    on.exit(if (file.exists(tfile)) file.remove(tfile))
    tfile
}

createCata <- function(tfile)
{
    cata <- function(...)
        cat(..., file = tfile, append = TRUE)
}

addCss <- function(file.name, cata, in.css.folder = TRUE)
{
    file.path <- if (in.css.folder)
        system.file("css", file.name, package = "flipFormat")
    else
        file.name

    if (file.exists(file.path))
    {
        cata("<style>\n")
        cata(readLines(file.path))
        cata("</style>\n\n")
    }
    else
        stop("CSS file ", file.path, " not found.")
}

addLeftPanel <- function(raw.and.normalized.text, cata)
{
    t <- cbind(rownames(raw.and.normalized.text), raw.and.normalized.text)
    names(t) <- c("", "Raw text", "Normalized text")

    cata("<div id=\"left-panel\">")
    cata(knitr::kable(t, align = c("c", "l", "l"),
                      format = "html",
                      table.attr = "class=\"text-analysis-table\""))
    cata("</div>") # end panel div
}

addRightPanel <- function(n.gram.frequencies, cata)
{
    t <- n.gram.frequencies
    names(t) <- c("n-grams", "#")

    cata("<div id=\"right-panel\">")
    cata(knitr::kable(t, align = c("l", "c"),
                      format = "html",
                      table.attr = "class=\"text-analysis-table\""))
    cata("</div>") # end panel div
}

createWidgetFromFile <- function(tfile)
{
    html <- paste(readLines(tfile), collapse = "\n")
    rhtmlMetro::Box(html, text.as.html = TRUE)
}
