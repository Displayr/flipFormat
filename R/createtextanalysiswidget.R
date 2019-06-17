#' Display text analysis output as an \code{htmlwidget}
#'
#' Creates a \code{htmlwidget} summary of diagnostic information for a
#' text analysis output from flipTextAnalysis.
#' @param raw.and.normalized.text A list containing the raw and normalized
#'   text. The first element, called "Original Text", is a character vector of
#'   the raw text. The second element, called "Transformed Text", is a list
#'   containing the identified phrases for each row. The optional third
#'   element, "Row Numbers", contains the row numers of the text. The optional
#'   fourth element, "Variable Start Indices", contains the a named numeric
#'   vector corresponding to the start indices of each variable in the text.
#' @param n.gram.frequencies A data frame with two variables, the first being
#'     the n-gram and the second being the frequencies.
#' @param token.substitutions A character matrix with two columns mapping the
#'      old tokens as they appeared in the original text (column 1) to the
#'      normalized tokens (column 2).
#' @param footer Character; footer to show at the bottom of the output.
#' @return An \code{htmlwidget} containing diagnostic information for
#'     the experimental design, including D-error, standard errors,
#'     frequenices, pairwise frequencies, the labeled design, and
#'     prior information. An attribute called \code{"ChartData"} also
#'     contains the labeled design.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
CreateTextAnalysisWidget <- function(raw.and.normalized.text,
                                     n.gram.frequencies,
                                     token.substitutions,
                                     footer = "")
{
    raw.and.normalized.text <- replaceMissingWithEmpty(raw.and.normalized.text)

    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("table.css", cata)
    addCss("textanalysis.css", cata)

    stylefile <- createTempFile()
    ws <- createCata(stylefile)
    colored.text <- HighlightNGrams(n.gram.frequencies, raw.and.normalized.text,
                                    token.substitutions, ws)
    if (NROW(n.gram.frequencies) > 0)
        addCss(stylefile, cata, in.css.folder = FALSE)

    cata("<div class=\"main-container\">")
    addLeftPanel(colored.text$text,
                 raw.and.normalized.text[["Row Numbers"]],
                 raw.and.normalized.text[["Variable Start Indices"]],
                 cata)
    addRightPanel(colored.text$n.grams, cata)

    cata("<div id=\"footer-container\">")
    cata(paste0("<p id=\"footer\">", footer,"</p>"))
    cata("</div>", fill = TRUE) # end footer-container div

    cata("</div>", fill = TRUE) # end main-container div

    createWidgetFromFile(tfile)
}

replaceMissingWithEmpty <- function(raw.and.normalized.text)
{
    original.text <- raw.and.normalized.text[["Original Text"]]
    original.text[is.na(original.text)] <- ""
    raw.and.normalized.text[["Original Text"]] <- original.text
    raw.and.normalized.text
}

#' @importFrom grDevices rgb col2rgb grey
#' @importFrom colorspace lighten darken
HighlightNGrams <- function(n.grams, text, subs, cata)
{
    col0 <- officialColors()
    n.col <- length(col0)
    bcol <- col0
    bs0 <- c("solid", "dashed", "dotted")
    bstyle <- apply(expand.grid(paste(bs0,bs0), paste(bs0,bs0)), 1, paste, collapse = " ")[c(2:4,6:8,9,5,1)]
    bshape <- c("border-radius: 50%; padding-left: 5px; padding-right: 5px; ",
                "padding-left: 1px; padding-right: 1px; ")

    # Set up vector of different styles for each phrase
    n <- nrow(n.grams)
    n.rep <- ceiling(n/n.col)
    i.offset <- 0.8/n.rep
    cc <- c(); bb <- c()
    bbi <- 0
    for (i in 0:(n.rep-1))
    {
        if (i > 0 && (i %% n.col) == 0)
            bbi <- bbi + 1
        cc <- c(cc, setAlpha(col0, 0.5))
        bb <- c(bb, paste0("border: 2px ", bcol[n.col - (i%%n.col)], "; ",
                    bshape[2 - (bbi%%2)], "border-style: ", bstyle[9 - (bbi%%9)], "; "))
    }
    borderstyles <- rep(bb, each = length(col0))[1:n]
    colors <- cc[1:n]

    n.grams[,1] <- as.character(n.grams[,1])
    n.grams <- data.frame(n.grams, num.var = 1)
    orig.text <- text[["Original Text"]]
    trans.tokens <- text[["Transformed Text"]]
    patt <- n.grams[,1]
    tooltips <- n.grams[,1]

    # Define CSS style for each ngram
    for (i in 1:n)
    {
        # Define CSS class
        cata(paste0(".word", i, "{ white-space: pre-wrap; ", borderstyles[i],
                    "line-height: 1.8em; background-color: ", colors[i], "; }\n"))

        # Create regex for replacement
        replace.ind <- which(subs[,2] == n.grams[i,1])
        n.grams[i,3] <- length(replace.ind)
        tooltips[i] <- paste(subs[replace.ind], collapse = ", ")
        if (length(replace.ind) == 1)
            patt[i] <- paste0("(", escWord(subs[replace.ind,1]), ")")
        else if (length(replace.ind) > 1)
        {
            replace.ind <- replace.ind[order(nchar(subs[replace.ind,1]), decreasing = TRUE)]
            patt[i] <- paste0("(", paste(escWord(subs[replace.ind,1]), sep="", collapse="|"), ")")
        }
    }

    # Search for ngrams in each response
    for (j in 1:length(orig.text))
    {
        if (sum(nchar(trans.tokens[[j]])) == 0)
            next

        # n.grams should be unique so a single index returned for each token in trans.tokens[[j]]
        ind <- match(trans.tokens[[j]], n.grams[,1])
        if (length(ind) == 0)
            next

        tmp.text <- orig.text[j]
        tmp.len <- nchar(tmp.text)
        tmp.pos <- 0
        new.text <- ""
        for (k in 1:length(trans.tokens[[j]]))
        {
            if (!is.na(ind[k]))
            {
                # Add formatting to transformed text
                tmp <- paste0("<span class=\"word", ind[k], "\">", trans.tokens[[j]][k], "</span>")
                trans.tokens[[j]][k] <- tmp

                # Add formatting to original text
                # We search through text in the same order as the tokens occur
                mpos <- regexpr(patt[ind[k]], substr(tmp.text, tmp.pos+1, tmp.len), ignore.case = TRUE, perl = TRUE)
                if (mpos != -1)
                {
                    mlen <- attr(mpos, "match.length")
                    new.text <- paste0(new.text,
                        substr(tmp.text, tmp.pos+1, tmp.pos+mpos-1),
                        "<span class=\"word", ind[k], "\">",
                        substr(tmp.text, tmp.pos+mpos, tmp.pos+mpos+mlen-1),
                        "</span>")
                    tmp.pos <- tmp.pos + mpos + mlen - 1
                }
            }
        }
        new.text <- paste0(new.text, substr(tmp.text, tmp.pos+1, tmp.len))
        orig.text[j] <- new.text
    }

    trans.text <- sapply(trans.tokens, paste, collapse = " ")

    # Create n-grams table with number of counts and variants
    # Tooltips is added via "title" - not related to the class CSS
    if (nrow(n.grams) > 0)
        n.grams[,1] <- paste0("<span class=\"word", 1:n, "\" title=\"", tooltips, "\">", n.grams[,1], "</span>")

    return(list(n.grams = n.grams,
                text = data.frame('Raw text' = orig.text, 'Normalized text' = trans.text,
                stringsAsFactors = FALSE)))

}

#  Escapes characters from pattern (e.g. '"', ''', '+').
#  This is needed in regular expressions unless 'fixed = TRUE' is used
#  Usually we only want to match whole words, but wordbreak ('\b')
#  does not match after special characters (i.e. not alphanumeric or underscore).
escWord <- function(x)
{
    prefix <- ifelse(grepl("^[a-zA-Z0-9_]", x, perl = TRUE), "\\b\\Q", "\\Q")
    suffix <- ifelse(grepl("[a-zA-Z0-0_]$", x, perl = TRUE), "\\E\\b", "\\E")
    return(paste0(prefix, x, suffix))
}

setAlpha <- function(col, alpha)
{
    vv <- col2rgb(col)
    return(paste0("rgba(", vv[1,], ",", vv[2,], ",",  vv[3,], ",", alpha, ")"))
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

#' @importFrom htmltools htmlEscape
addLeftPanel <- function(raw.and.normalized.text, row.numbers,
                         variable.start.indices, cata)
{
    t.rownames <- if (!is.null(row.numbers))
        row.numbers
    else
        rownames(raw.and.normalized.text)

    cata("<div id=\"left-panel\">")

    if (!is.null(variable.start.indices))
    {
        variable.end.indices <- c(variable.start.indices[-1] - 1,
                                  nrow(raw.and.normalized.text))
        for (i in 1:length(variable.start.indices))
        {
            ind <- variable.start.indices[i]:variable.end.indices[i]
            t <- cbind(t.rownames[ind], raw.and.normalized.text[ind, ])
            names(t) <- c("", "Raw text", "Normalized text")
            rownames(t) <- NULL
            cata("<span class=\"variable-name\">",
                 htmlEscape(names(variable.start.indices)[i]),
                 "</span>")
            cata(knitr::kable(t, align = c("c", "l", "l"), format = "html",
                              escape = FALSE,
                              table.attr = "class=\"text-analysis-table\""))
        }
    }
    else
    {
        t <- cbind(t.rownames, raw.and.normalized.text)
        names(t) <- c("", "Raw text", "Normalized text")
        rownames(t) <- NULL
        cata(knitr::kable(t, align = c("c", "l", "l"), format = "html",
                          escape = FALSE,
                          table.attr = "class=\"text-analysis-table\""))
    }

    cata("</div>") # end panel div
}

addRightPanel <- function(n.gram.frequencies, cata)
{
    t <- n.gram.frequencies
    names(t) <- c(paste0("Phrases (", nrow(n.gram.frequencies), ")"), "Frequency", "# Variants")

    cata("<div id=\"right-panel\">")
    cata(knitr::kable(t, align = c("l", "c", "c"),
                      format = "html", escape = FALSE,
                      table.attr = "class=\"text-analysis-table\""))
    cata("</div>") # end panel div
}

createWidgetFromFile <- function(tfile)
{
    html <- paste(readLines(tfile), collapse = "\n")
    rhtmlMetro::Box(html, text.as.html = TRUE)
}
