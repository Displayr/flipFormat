#' Display text analysis output as an \code{htmlwidget}
#'
#' Creates a \code{htmlwidget} summary of diagnostic information for a
#' text analysis output from flipTextAnalysis.
#' @param raw.and.normalized.text A data frame with two variables, the first
#'     being the raw text and the second being the normalized text. The row
#'     names of the data frame should correspond to the case index.
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
    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("table.css", cata)
    addCss("textanalysis.css", cata)

    stylefile <- createTempFile()
    ws <- createCata(stylefile)
    colored.text <- HighlightNGrams(n.gram.frequencies, raw.and.normalized.text,
                                    token.substitutions, ws)
    addCss(stylefile, cata, in.css.folder = FALSE)

    cata("<div class=\"main-container\">")
    addLeftPanel(colored.text$text, cata)
    addRightPanel(colored.text$n.grams, cata)

    cata("<div id=\"footer-container\">")
    cata(paste0("<p id=\"footer\">", footer,"</p>"))
    cata("</div>", fill = TRUE) # end footer-container div

    cata("</div>", fill = TRUE) # end main-container div

    createWidgetFromFile(tfile)
}

#' @importFrom grDevices rgb col2rgb grey
#' @importFrom colorspace lighten darken
HighlightNGrams <- function(n.grams, text, subs, cata)
{
    col0 <- c("#3e7dcc", "#04b5ac", "#f5c524", "#c44e41",
              "#8cc0ff", "#ff905a", "#345e8c",
              "#04827b", "#967f47","#96362f")
    n.col <- length(col0)
    bcol <- col0
    bstyle <- c("dotted", "dashed", "solid")
    n <- nrow(n.grams)
    n.rep <- ceiling(n/n.col)
    i.offset <- 0.8/n.rep
    cc <- c(); bb <- c()
    for (i in 0:(n.rep-1))
    {
        if (i > 0 && i %% n.col == 0)
            bcol <- lighten(bcol)
        cc <- c(cc, setAlpha(col0, 0.5))
        bb <- c(bb, paste("2px", bstyle[3-(i%%3)], bcol[n.col - (i%%n.col)]))
    }
    borderstyles <- rep(bb, each = length(col0))[1:n]
    colors <- cc[1:n]

    n.grams[,1] <- as.character(n.grams[,1])
    orig.text <- text[[1]]
    trans.tokens <- text[[2]]
    for (i in 1:n)
    {
        # Define CSS class
        cata(paste0(".word", i, "{ white-space: pre-wrap; border: ", borderstyles[i],
                    "; line-height: 1.8em; background-color: ", colors[i], "; }\n"))

        # Look for exact matches in transformed text (which is already split into tokens)
        ind <- which(sapply(trans.tokens, function(x){any(x == n.grams[i,1])}))
        for (ii in ind)
        {
            pos <- which(trans.tokens[[ii]] == n.grams[i,1])
            tmp <- paste0("<span class=\"word", i, "\">", trans.tokens[[ii]][pos], "</span>")
            trans.tokens[[ii]][pos] <- tmp
        }

        # Look for matches including corrections and manual replacements in original text
        replace.ind <- which(subs[,2] == n.grams[i,1])
        if (length(replace.ind) == 1)
        {
            patt <- escWord(subs[replace.ind,2])
            orig.text[ind] <- gsub(paste0("\\b", patt, "\\b"),
                          paste0("SPAN_DELIM_OPEN_", i, "\">", patt, "SPAN_DELIM_CLOSE"), orig.text[ind],
                          ignore.case = TRUE)
        } else
        {
            patt <- paste0("(", paste(escWord(subs[replace.ind,1]), sep="", collapse="|"), ")")
            #cat(i, "patt:", patt, "\n")
            if (nchar(patt) <= 2)
                warning("Empty regular expression for ", n.grams[i,1], " ignored.")
            else
                orig.text[ind] <- gsub(paste0("\\b", patt, "\\b"),
                          paste0("SPAN_DELIM_OPEN_", i, "\">", "\\1", "SPAN_DELIM_CLOSE"), orig.text[ind],
                          ignore.case = TRUE)
        }
    }
    # finish off substitutions - we use this two step process to avoid problems
    # if the n-gram matches 'span' or 'class'
    orig.text <- gsub("SPAN_DELIM_OPEN_", "<span class=\"word", orig.text)
    orig.text <- gsub("SPAN_DELIM_CLOSE", "</span>", orig.text)
    trans.text <- sapply(trans.tokens, paste, collapse = " ")

    n.grams[,1] <- paste0("<span class=\"word", 1:n, "\">", n.grams[,1], "</span>")
    return(list(n.grams = n.grams,
                text = data.frame('Raw text' = orig.text, 'Normalized text' = trans.text,
                            stringsAsFactors = FALSE)))

}

#  Escapes characters from pattern (e.g. '"', ''', '+').
#  This is needed in regular expressions unless 'fixed = TRUE' is used
#' @importFrom stringr str_replace_all
escWord <- function(x)
{
    return(str_replace_all(x, "(\\W)", "\\\\\\1"))
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

addLeftPanel <- function(raw.and.normalized.text, cata)
{
    t.rownames <- rownames(raw.and.normalized.text)
    t <- cbind(t.rownames, raw.and.normalized.text)
    names(t) <- c("", "Raw text", "Normalized text")
    rownames(t) <- NULL

    cata("<div id=\"left-panel\">")
    cata(knitr::kable(t, align = c("c", "l", "l"),
                      format = "html", escape = FALSE,
                      table.attr = "class=\"text-analysis-table\""))
    cata("</div>") # end panel div
}

addRightPanel <- function(n.gram.frequencies, cata)
{
    t <- n.gram.frequencies
    names(t) <- c("n-grams", "#")

    cata("<div id=\"right-panel\">")
    cata(knitr::kable(t, align = c("l", "c"),
                      format = "html", escape = FALSE,
                      table.attr = "class=\"text-analysis-table\""))
    cata("</div>") # end panel div
}

createWidgetFromFile <- function(tfile)
{
    html <- paste(readLines(tfile), collapse = "\n")
    rhtmlMetro::Box(html, text.as.html = TRUE)
}
