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
    bs0 <- c("solid", "dashed", "dotted")
    bstyle <- apply(expand.grid(paste(bs0,bs0), paste(bs0,bs0)), 1, paste, collapse = " ")[c(2:4,6:8,9,5,1)]
    bshape <- c("border-radius: 50%; padding-left: 5px; padding-right: 5px; ",
                "padding-left: 1px; padding-right: 1px; ")

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
    orig.text <- text[[1]]
    trans.tokens <- text[[2]]
    ngram.order <- order(nchar(n.grams[,1]), decreasing = TRUE)
    patt <- n.grams[,1]

    # Define CSS style for each ngram
    for (i in ngram.order)
    {
        # Define CSS class
        cata(paste0(".word", i, "{ white-space: pre-wrap; ", borderstyles[i],
                    "line-height: 1.8em; background-color: ", colors[i], "; }\n"))

        # Create regex for replacement
        replace.ind <- which(subs[,2] == n.grams[i,1])
        prefix <- "\\b("
        suffix <- ")\\b"
        if (any(grepl("^\\W", subs[replace.ind,1])))
            suffix <- "("
        if (any(grepl("\\W$", subs[replace.ind,1])))
            suffix <- ")"

        if (length(replace.ind) == 1)
            patt[i] <- paste0(prefix, escWord(subs[replace.ind,1]), suffix)
        else if (length(replace.ind) > 1)
        {
            replace.ind <- replace.ind[order(nchar(subs[replace.ind,1]), decreasing = TRUE)]
            patt[i] <- paste0(prefix, "(", paste(escWord(subs[replace.ind,1]), sep="", collapse="|"), 
                        ")", suffix)
        }
    }

    # Search for ngrams in each response
    for (j in 1:length(orig.text))
    {
        # n.grams should be unique so a single index returned for each token in trans.tokens[[j]]
        ind <- match(trans.tokens[[j]], n.grams[,1])
        if (length(ind) == 0)
            next

        orig.tmp <- orig.text[j]
        for (k in 1:length(trans.tokens[[j]]))
        {
            if (!is.na(ind[k]))
            {
                # Add formatting to transformed text
                tmp <- paste0("<span class=\"word", ind[k], "\">", trans.tokens[[j]][k], "</span>")
                trans.tokens[[j]][k] <- tmp

                # Add formatting to original text. We search through text in the same order as
                # the tokens occur. Previous substitutions should not match because the
                # SPAN_DELIM tags will not satisfy the '\b' (word break) pattern
                orig.tmp <- sub(patt[ind[k]],   #paste0("\\b(", patt[ind[k]], ")"),
                          paste0("SPAN_DELIM_OPEN_", ind[k], "\">", "\\1", "SPAN_DELIM_CLOSE"), orig.tmp,
                          ignore.case = TRUE, perl = TRUE)
            }
        }
        # finish off substitutions - we use this two step process to avoid problems
        # if the n-gram matches 'span' or 'class'
        orig.tmp <- gsub("SPAN_DELIM_OPEN_", "<span class=\"word", orig.tmp)
        orig.text[j] <- gsub("SPAN_DELIM_CLOSE", "</span>", orig.tmp)
    }

    trans.text <- sapply(trans.tokens, paste, collapse = " ")
    n.grams[,1] <- paste0("<span class=\"word", 1:n, "\">", n.grams[,1], "</span>")
    return(list(n.grams = n.grams,
                text = data.frame('Raw text' = orig.text, 'Normalized text' = trans.text,
                            stringsAsFactors = FALSE)))

}

#  Escapes characters from pattern (e.g. '"', ''', '+').
#  This is needed in regular expressions unless 'fixed = TRUE' is used
escWord <- function(x)
{
    return(paste0("\\Q", x, "\\E"))
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
    names(t) <- c("Phrases", "#")

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
