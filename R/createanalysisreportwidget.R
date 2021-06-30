#' Display widget for analysis reports for Q Trees
#'
#' Produces an \code{htmlwidget} containing the analysis report(s)
#' from a Mixed-Mode Trees, Latent Class Analysis, or Mixed-Mode
#' Cluster Analysis run in Q or Displayr.
#' @param x A character vector with each element containing a string
#'     in rtf format, an analysis report from tree model fit in Q or
#'     Displayr.
#' @param css Optional string giving the path to a file containing
#'     additional CSS to style the htmlwidget output.
#' @param ... Currently, ignored.
#' @return An \code{htmlwidget} containing the analysis report with
#'     some simple formatting.
#' @seealso \code{\link[striprtf]{read_rtf}},\code{\link[rhtmlMetro]{Box}}
#' @export
#' @importFrom striprtf read_rtf
CreateAnalysisReportWidget <- function(x,
                                          css = NULL,
                                          ...)
{
    tfile <- createTempFile()

    cata <- createCata(tfile)
    format1 <- function(x)
        format(x, nsmall = nsmall, digits = digits)

    ## Use same styling as our Choice Modeling - Experimental Design widghet
    addCss("analysisreport.css", cata)
    if (!is.null(css))
      addCss(css, cata, in.css.folder = FALSE)

    ## Needed so that Box has scollbar
    cata("<div class=\"analysis-report-main-container\">")

    ## Title
    chart.data <- NULL
    for (i in seq_along(x))
    {
        rtf.file <- tempfile(fileext = ".rtf")
        writeLines(x[i], rtf.file)
        parsed <- read_rtf(rtf.file, check_file = FALSE)
        chart.data <- c(chart.data, "\n\n", parsed)
        if (i == 1)
        {
            obj.idx <- grep("^Objective:", parsed)
            type <- switch(sub("Objective: ", "", parsed[obj.idx]),
                           Mixture = "Latent Class Analysis",
                           Discrete = "Mixed-Mode Tree",
                           Clustering = "Mixed-Mode Cluster Analysis")
            cata("<h1>", type, "</h1>")
        }
        if (type == "Mixed-Mode Tree")
            cata("<details open=\"true\" class=\"choice-modelling-design-details\">",
                 "<summary class=\"choice-modelling-design-summary\">",
                 "Branch ", i,
                 "</summary>\n")
        addReport(parsed, cata)
        if (type == "Mixed-Mode Tree")
            cata("</details>\n")
    }
    cata("</div>\n")

    html <- paste(readLines(tfile), collapse = "\n")
    ## browseURL(tfile)
    out <- boxIframeless(html, text.as.html = TRUE,
                         font.family = "Circular, Arial, sans-serif",
                         background.color = "White",
                         font.size = 8)

    attr(out, "ChartData") <- convertReportVectorToMatrix(chart.data)
    return(out)
}

addReport <- function(report, outputWriter)
{
    table.indices <- grep("^\\*\\|", report)
    start.idx <- c(1, which(diff(table.indices) > 1) + 1)
    end.idx <- c(which(diff(table.indices) > 1), length(table.indices))
    table.end.indices <- table.indices[end.idx]
    table.headers <- table.indices[start.idx]
    table.titles <- table.headers - 1

    report[table.headers] <- sub("^\\*\\|", "<table>\n<thead>\n  <tr>\n    <th>",
                                 report[table.headers])
    report[table.headers] <- gsub("|", "</th><th>", report[table.headers], fixed = TRUE)
    report[table.headers] <- paste0(report[table.headers], "\n  </tr>\n</thead>\n<tbody>")

    table.row.indices <- table.indices[-start.idx]
    report[table.row.indices] <- sub("^\\*\\|", "<tr>\n    <td>",
                                 report[table.row.indices])
    report[table.row.indices] <- gsub("|", "</td><td>", report[table.row.indices],
                                      fixed = TRUE)

    report[table.end.indices] <- paste0(report[table.end.indices],
                                        "</td>\n  </tbody>\n</table>")

    blank.idx <- which(!nzchar(report))
    report[blank.idx] <- "</br>"
    SECTIONS <- c("Grow Settings", "Analysis Report", "Detailed history")
    section.idx <- which(report %in% SECTIONS)
    report[section.idx] <- paste0("<h2>", report[section.idx], "</h2>")

    SUBSECTIONS <- c("Question-specific assumptions:",
                     "Number of Responses", "Convergence data",
                     "Fit statistics", "Class Priors", "Class Sizes", "Class Parameters")
    subsection.idx <- which(report %in% SUBSECTIONS)
    report[subsection.idx] <- paste0("<h3>", report[subsection.idx], "</h3>")

    title.idx <- setdiff(table.titles, c(section.idx, subsection.idx))
    report[title.idx] <- paste0("<p><b>", report[title.idx], "</b></p>")

    remaining.idx <- -c(blank.idx, table.indices, section.idx, subsection.idx)
    report[remaining.idx] <- paste0("<p>", report[remaining.idx], "</p>")
    report[length(report)] <- paste0(report[length(report)], "\n")
    outputWriter(report)
    invisible()
}

#' Convert read_rtf output to character matrix
#'
#' @param report Character vector output from read_rtf
#' @return A character matrix with any elements of \code{report}
#' containing table rows converted from a single string to
#' a character vector/row in the output. Elements of report that
#' don't contain a table are left unchanged
#' @noRd
convertReportVectorToMatrix <- function(report)
{
    table.indices <- grep("^\\*\\|", report)
    report[table.indices] <- sub("^\\*\\|", "", report[table.indices])
    table.cells <- strsplit(report[table.indices], " | ", fixed = TRUE)
    max.len <- max(vapply(table.cells, length, 1L))
    out <- matrix("", length(report), max.len,
                  dimnames = list(character(length(report)), character(max.len)))
    out[, 1] <- report
    for (i in seq_along(table.indices))
    {
        row.idx <- table.indices[i]
        row <- table.cells[[i]]
        out[row.idx, seq_along(row)] <- row
    }
    return(out)
}
