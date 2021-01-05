#' Display choice model design output as an \code{htmlwidget}
#'
#' Creates a \code{htmlwidget} summary of diagnostic information for a
#' choice model design output from \code{\link[flipChoice]{ChoiceModelDesign}}.
#' @param x An object of class \code{\link[flipChoice]{ChoiceModelDesign}} output from
#' \code{\link[flipChoice]{ChoiceModelDesign}}.
#' @param css Optional string giving the path to a file containing additional
#' CSS to style the htmlwidget output.
#' @param nsmall Integer; see \code{\link{format}}.
#' @param digits Integer; see \code{\link{format}}.
#' @param ... Currently, ignored.
#' @return An \code{htmlwidget} containing diagnostic information for
#'     the experimental design, including D-error, standard errors,
#'     frequenices, pairwise frequencies, the labeled design, and
#'     prior information. An attribute called \code{"ChartData"} also
#'     contains the labeled design.
#' @seealso \code{\link[flipChoice]{ChoiceModelDesign}},
#'     \code{\link[rhtmlMetro]{Box}}
#' @export
#' @importFrom knitr kable
#' @importFrom htmltools tagList tags includeCSS browsable
#' @importFrom htmlwidgets onRender
#' @importFrom rhtmlMetro Box
CreateChoiceModelDesignWidget <- function(x,
                                          css = NULL,
                                          nsmall = 3,
                                          digits = 2,
                                          ...)
{
    tfile <- createTempFile()
    b.o <- x$balances.and.overlaps

    cata <- createCata(tfile)
    format1 <- function(x)
        format(x, nsmall = nsmall, digits = digits)

    ## add CSS
    addCss("table.css", cata)
    addCss("cmd.css", cata)
    addCss("details.css", cata)
    if (!is.null(css))
      addCss(css, cata, in.css.folder = FALSE)

    ## Needed so that Box has scollbar
    cata("<div class=\"choice-modelling-design-main-container\">")

    ## Title
    cata("<h1>Choice Model: Experimental Design</h1>")

    ## statistics
    addStatistics(x, digits, nsmall, cata)

    ## Standard errors
    if (!is.null(x$standard.errors))
    {
        cata("<details open=\"true\" class=\"details\"><summary class=\"summary\">Standard Errors</summary>\n")
        cata(makeStandardErrorTable(x$standard.errors, x$attribute.levels,
                                    digits = digits, nsmall = nsmall), fill = TRUE)
        cata("</details>")
    }

    ## Frequencies
    has.const.attr <- !is.null(x$n.constant.attributes) && x$n.constant.attributes > 0
    cata("<details open=\"true\" class=\"details\"><summary class=\"summary\">Frequencies</summary>\n")
    cata(makeFrequencyTable(b.o$singles, has.const.attr), fill = TRUE)
    cata("</details>")

    ## Pairwise frequencies
    cata("<details open =\"true\" class=\"details\"><summary class=\"summary\">Pairwise Frequencies</summary>\n")
    mapply(addPairwiseFrequencyTable, b.o$pairs, names(b.o$pairs),
           MoreArgs = list(attr.names = names(x$attribute.levels), tfile = tfile))
    cata("</details>")

    ## Overlaps (N/A for alt. specific designs)
    if (!is.null(b.o$overlaps))
        addOverlaps(b.o$overlaps, cata)

    ## Design
    cata("<details class=\"details\"><summary class=\"summary\">Design</summary>\n")
    cata(knitr::kable(x$labeled.design, align = "c",
                      longtable = TRUE, format = "html", digits = digits),
         fill = TRUE)
    cata("</details>")

    ## Priors
    cata("<details open =\"true\" class=\"details\"><summary class=\"summary\">Prior</summary>\n")
    if (!is.null(x$prior))
        cata(makePriorTable(x$prior, x$attribute.levels, digits, nsmall), fill = TRUE)
    else
        cata("<p>None specified.</p></details>")
    cata("</div>", fill = TRUE)

    html <- paste(readLines(tfile), collapse = "\n")
    out <- rhtmlMetro::Box(html, text.as.html = TRUE,
                    font.family = "Circular, Arial, sans-serif",
                    font.size = 8)

    attr(out, "ChartData") <- x$labeled.design
    return(out)
}

addStatistics <- function(x, digits, nsmall, cata)
{
    format1 <- function(x)
        format(x, nsmall = nsmall, digits = digits)

    b.o <- x$balances.and.overlaps
    cata("<details open=\"true\" class=\"details\">\n")
    cata("<summary class=\"summary\">Statistics</summary>")
    ## cata("<p style=\"text-align: left;\">")
    ## cata(paste0("<b>D-error: </b>", format1(x$d.error)))
    ## cata(paste0("<span style=\"float: right;\">",
    ##             "<b>Mean version balance: </b>",
    ##             format1(b.o$mean.version.balance),
    ##             "</span></p>\n"))
    ## cata(paste0("<b>Across version balance: </b>",
    ##             format1(b.o$across.version.balance)))
    ## cata(paste0("<span style=\"float: right;\">",
    ##             "<b>Mean version balance: </b>",
    ##             format1(b.o$mean.version.balance),
    ##             "</span></p>\n"))
    cata("<table id = \"cmd-table-diagnostics\"><tbody><tr>\n")
    cata("<td style=\"text-align: left;\">")
    cata(paste0("<b>Algorithm: </b>", x$design.algorithm, "</td>"))
    cata("<td style=\"text-align: left;\">")
    cata(paste0("<b>D-error: </b>", format1(x$d.error), "</td>"))
    if (!is.null(x$a.error))
    {
        cata("<td style=\"text-align: left;\">")
        cata(paste0("<b>A-error: </b>", format1(x$a.error), "</td>"))
    }
    cata("</tr>\n")
    if (!is.null(b.o$mean.version.balance))
    {  # not available for partial profiles with constant attributes
        cata(paste0("<tr><td style=\"text-align: left;\">",
                    "<b>Mean version balance: </b>",
                    format1(b.o$mean.version.balance),
                    "</td>\n"))
        cata(paste0("<td style=\"text-align: left;\">",
                    "<b>Across version balance: </b>",
                    format1(b.o$across.version.balance),
                    "</td>\n"))
        if (!is.null(x$a.error))
            cata("<td></td>\n")
        cata("</tr>\n")
    }

    if (!is.null(b.o$across.version.pairwise.balance))
    {  # not available for partial profiles with constant attributes
      cata(paste0("<tr><td style=\"text-align: left;\">",
                  "<b>Mean version pairwise balance: </b>",
                  format1(b.o$mean.version.pairwise.balance),
                  "</td>\n"))
      cata(paste0("<td style=\"text-align: left;\">",
                  "<b>Across version pairwise balance: </b>",
                  format1(b.o$across.version.pairwise.balance),
                  "</td>\n"))
      if (!is.null(x$a.error))
          cata("<td></td>\n")
      cata("</tr>\n")
    }
    cata("</tbody></table></details>\n\n")
    invisible()
}

#' @importFrom knitr kable
makeStandardErrorTable <- function(std.err, al, digits, nsmall)
{
    zero.str <- ".000"
    format1 <- function(x)
        sub("^0[.]", ".", format(x, nsmall = nsmall, digits = digits))
    max.len <- max(vapply(al, length, 0L))

    out <- matrix("", nrow = max.len, ncol = 2*length(al))
    cnames <- character(ncol(out))
    cnames[seq(1, ncol(out), by = 2)] <- names(al)
    idx <- 1
    for (i in seq_along(al))
    {
        lvls <- al[[i]]
        n.lvls <- length(lvls)
        out[seq_len(n.lvls), 2*(i-1)+1] <- lvls
        out[seq_len(n.lvls), 2*i] <- c(zero.str, format1(std.err[idx:(idx+n.lvls - 2), 2]))
        idx <- idx + n.lvls-1
    }
    out <- knitr::kable(out, format = "html", col.names = cnames, digits = digits,
                 table.attr = "class=\"cmd-table-one-stat\"",
                 align = rep(c("l", "r"), length(al)))
    ## change table headers to span multiple columns
    out <- gsub("<th style=\"text-align:right;\">  </th>", "", out, fixed = TRUE)
    out <- gsub("<th style=\"text-align:left;\">",
                "<th colspan = \"2\">", out, fixed = TRUE)
    return(out)
}

#' @param freq A list of frequencies/counts that each level
#' appears in a choice model design; the number of elements
#' in the list is the number of attributes in the design
#' @param const.attr whether the design is a partial profiles
#' design with constant attributes
#' @importFrom knitr kable
#' @noRd
makeFrequencyTable <- function(freq, const.attr = FALSE)
{
    max.idx <- which.max(vapply(freq, length, 0L))
    long.freq <- freq[[max.idx]]
    max.len <- length(freq[[max.idx]])
    if (const.attr && names(long.freq)[length(long.freq)] != "Not shown")
        max.len <- max.len + 1

    out <- matrix(" ", nrow = max.len, ncol = 2*length(freq))
    if (const.attr)
    {
        out[max.len, seq.int(1, ncol(out), by = 2)] <- "<em>Not shown</em>"
        out[max.len, seq.int(2, ncol(out), by = 2)] <- "0"
    }

    cnames <- character(ncol(out))
    cnames[seq(1, ncol(out), by = 2)] <- names(freq)
    for (i in seq_along(freq))
    {
        lvls <- names(freq[[i]])
        n.lvls <- length(lvls)
        if (const.attr && lvls[n.lvls] == "Not shown")
        {  # Make sure "Not shown" counts appear in bottom row
            out[max.len, 2*i] <- freq[[i]][n.lvls]
            out[seq_len(n.lvls-1), 2*(i-1)+1] <- lvls[-n.lvls]
            out[seq_len(n.lvls-1), 2*i] <- freq[[i]][-n.lvls]
        }else
        {
            out[seq_len(n.lvls), 2*(i-1)+1] <- lvls
            out[seq_len(n.lvls), 2*i] <- freq[[i]]

        }
    }
    out <- knitr::kable(out, format = "html", col.names = cnames, align = "c",
                        table.attr = "class=\"cmd-table-one-stat\"", escape = FALSE)
    ## center column headers across two columns
    out <- gsub("<th style=\"text-align:center;\">  </th>", "", out, fixed = TRUE)
    out <- gsub("<th style=\"text-align:center;\">",
                "<th colspan = \"2\">", out, fixed = TRUE)
    return(out)
}

addPairwiseFrequencyTable <- function(tfile, ptable, table.name, attr.names)
{
    cata <- function(...)
        cat(..., file = tfile, append = TRUE)

    ## table.names always has form "attr.name1/attr.name2"
    ## be overly cautious to extract the two names w/o strsplit
    ## idx1 <- vapply(attr.names, function(n) grepl(paste0("^", n, "[/]"), table.name), FALSE)
    ## idx2 <- vapply(attr.names, function(n) grepl(paste0("[/]", n, "$"), table.name), FALSE)

    cata("<details open=\"true\" class=\"details\">")
    cata(paste0("<summary class=\"cmd-summary-pairwise summary\">", table.name, "</summary>\n"))
    cata(knitr::kable(ptable, row.names = TRUE, col.names = colnames(ptable),
                      format = "html", align = "c",
                      table.attr = "class=\"cmd-table-pairwise\""), fill = TRUE)
    cata("</details>")
    invisible()
}

#' @importFrom knitr kable
addOverlaps <- function(overlaps, cata)
{
    cata("<details open=\"true\" class=\"details\">")
    cata("<summary class=\"summary\">Overlaps</summary>")
    cata(knitr::kable(t(overlaps), col.names = names(overlaps), align = "c",
                      format = "html", table.attr = "id=\"cmd-table-overlaps\""),
         fill = TRUE)
    cata("</details>")
    invisible()
}

#' @importFrom knitr kable
makePriorTable <- function(prior, al, digits = 2, nsmall = 2)
{
    zero.str <- ".000"
    format1 <- function(x)
        sub("^0[.]", ".", format(x, nsmall = nsmall, digits = digits))
    n.lvls <- vapply(al, length, 0L)
    max.len <- max(n.lvls)
    sd.prior.given <- is.matrix(prior)
    prior <- as.matrix(prior)

    n.col <- ifelse(sd.prior.given, 3*length(al), 2*length(al))
    out <- matrix("", nrow = max.len, ncol = n.col)
    cnames <- character(ncol(out))
    if (sd.prior.given)
    {
        cnames[seq(1, ncol(out), by = 3)] <- names(al)
        cnames[seq(2, ncol(out), by = 3)] <- "Mean"
        cnames[seq(3, ncol(out), by = 3)] <- "Std. Dev."
        table.attr <- "class=\"cmd-table-two-stat\""
        col.span <- "3"
        col.align <- c("l", "r", "r")
    }else
    {
        cnames[seq(1, ncol(out), by = 2)] <- names(al)
        table.attr <- "class=\"cmd-table-one-stat\""
        col.span <- "2"
        col.align <- c("l", "r")
    }

    idx <- 1
    for (i in seq_along(al))
    {
        lvls <- al[[i]]
        n.lvls <- length(lvls)
        col.idx <- ifelse(sd.prior.given, 3*(i-1)+1, 2*(i-1)+1)
        out[seq_len(n.lvls), col.idx] <- lvls
        col.idx <- ifelse(sd.prior.given, 3*(i-1)+2, 2*i)
        out[seq_len(n.lvls), col.idx] <- c(zero.str, format1(prior[idx:(idx+n.lvls - 2), 1]))
        if (sd.prior.given)
            out[seq_len(n.lvls), 3*i] <- c(zero.str, format1(prior[idx:(idx+n.lvls - 2), 2]))
        idx <- idx + n.lvls-1
    }
    out <- knitr::kable(out, format = "html", col.names = cnames,
                 table.attr = table.attr,
                 align = rep(col.align, length(al)))
    if (!sd.prior.given)
    {
        out <- gsub("<th style=\"text-align:right;\">  </th>", "", out, fixed = TRUE)
        out <- gsub("<th style=\"text-align:left;\">",
                    paste0("<th colspan = \"", col.span, "\">"), out, fixed = TRUE)

    }
    return(out)
}
