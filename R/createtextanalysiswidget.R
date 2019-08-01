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
#'   the n-gram and the second being the frequencies.
#' @param token.substitutions A character matrix with two columns mapping the
#'   old tokens as they appeared in the original text (column 1) to the
#'   normalized tokens (column 2).
#' @param footer Character; footer to show at the bottom of the output.
#' @param diagnostics A list containing diagnostic information to be shown in
#'   the diagnostic output.
#' @return An \code{htmlwidget} containing tables showing the output from a
#'   text analysis.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
CreateTextAnalysisWidget <- function(raw.and.normalized.text,
                                     n.gram.frequencies,
                                     token.substitutions,
                                     footer = "",
                                     diagnostics = NULL)
{
    raw.and.normalized.text <- replaceMissingWithEmpty(raw.and.normalized.text)

    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("table.css", cata)
    addCss("details.css", cata)
    addCss("textanalysis.css", cata)

    stylefile <- createTempFile()
    ws <- createCata(stylefile)
    colored.text <- HighlightNGrams(n.gram.frequencies, raw.and.normalized.text,
                                    token.substitutions, ws)
    if (NROW(n.gram.frequencies) > 0)
        addCss(stylefile, cata, in.css.folder = FALSE)

    cata("<div class=\"main-container\">")
    cata("<div class=\"vertical-container\">")

    addTopPanel(cata, colored.text,
                raw.and.normalized.text)

    if (!is.null(diagnostics))
        addDiagnosticsPanel(cata, diagnostics)

    cata("</div>", fill = TRUE) # end vertical-container div

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
#' @importFrom flipU UniquePlaceholders
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
    n.grams <- data.frame(n.grams, num.var = rep(1, n))
    orig.text <- text[["Original Text"]]
    trans.tokens <- text[["Transformed Text"]]
    patt <- n.grams[,1]
    tooltips <- n.grams[,1]

    # Define CSS style for each ngram
    for (i in seq_len(n))
    {
        # Define CSS class
        if (n.grams[i,1] == "UNCLASSIFIED")
            cata(paste0(".word", i, "{ white-space: pre-wrap; ",
                    "line-height: 1.8em; background-color: #CCCCCC; }\n"))
        else
            cata(paste0(".word", i, "{ white-space: pre-wrap; ", borderstyles[i],
                    "line-height: 1.8em; background-color: ", colors[i], "; }\n"))

        # Create regex for replacement
        replace.ind <- which(subs[,2] == n.grams[i,1])
        tmp.subs <- unique(subs[replace.ind,1]) # group different capitalizations counted separately
        n.grams[i,3] <- length(tmp.subs)
        tooltips[i] <- paste(escapeQuotesForHTML(tmp.subs), collapse = ", ")
        if (length(replace.ind) == 1)
            patt[i] <- paste0("(", escWord(subs[replace.ind,1]), ")")
        else if (length(replace.ind) > 1)
        {
            replace.ind <- replace.ind[order(nchar(subs[replace.ind,1]), decreasing = TRUE)]
            patt[i] <- paste0("(", paste(escWord(subs[replace.ind,1]), sep="", collapse="|"), ")")
        }
        else
            patt[i] <- escWord(patt[i])
    }

    raw.replacement.rows <- sapply(text$`Raw replacement info`,
                                   function(x) x$row.index)

    # Search for ngrams in each response
    for (j in 1:length(orig.text))
    {
        trans.tokens.j <- trans.tokens[[j]]
        if (sum(nchar(trans.tokens.j)) == 0)
            next

        # n.grams should be unique so a single index returned for each token in trans.tokens[[j]]
        ind <- match(trans.tokens.j, n.grams[,1])
        if (length(ind) == 0)
            next

        # Raw text

        # first replace raw replacements with placeholders
        raw.repl.ind <- raw.replacement.rows == j
        if (sum(raw.repl.ind) > 0)
        {
            raw.repl <- text$`Raw replacement info`[raw.repl.ind]
            n.raw.repl <- length(raw.repl)
            raw.repl.placeholders <- UniquePlaceholders(n.raw.repl,
                                                        padding = "-")
            ord <- order(sapply(raw.repl, function(x) x$start.end[1]))
            raw.repl <- raw.repl[ord]
            start.ind <- sapply(raw.repl, function(x) x$start.end[1])
            end.ind <- sapply(raw.repl, function(x) x$start.end[2])

            new.text <- character(0)
            new.text <- paste0(new.text,
                               substr(orig.text[j], 1, start.ind[1] - 1))
            for (i in seq_len(n.raw.repl))
            {
                new.text <- paste0(new.text, raw.repl.placeholders[i])
                if (i < n.raw.repl)
                    new.text <- paste0(new.text,
                                       substr(orig.text[j], end.ind[i] + 1,
                                              start.ind[i + 1] - 1))
                else
                    new.text <- paste0(new.text,
                                       substr(orig.text[j], end.ind[i] + 1,
                                              nchar(orig.text[j])))
            }
        }
        else
        {
            raw.repl <- list()
            new.text <- orig.text[j]
        }

        # replace tokens with placeholders
        n.tokens <- length(trans.tokens.j)
        raw.token.tags <- character(0)
        token.placeholders <- character(0)
        for (k in 1:n.tokens)
        {
            if (!is.na(ind[k]))
            {
                mpos <- regexpr(patt[ind[k]], new.text,
                                ignore.case = TRUE, perl = TRUE)
                if (mpos != -1)
                {
                    raw.token <- substr(new.text, mpos,
                                        mpos + attr(mpos, "match.length") - 1)
                    raw.token.tags <- c(raw.token.tags,
                                        paste0("<span class=\"word", ind[k],
                                               "\">", raw.token, "</span>"))
                    placeholder <- UniquePlaceholders(1, padding = "-")
                    token.placeholders <- c(token.placeholders, placeholder)
                    new.text <- sub(patt[ind[k]], placeholder,
                                    new.text, ignore.case = TRUE, perl = TRUE)
                }
            }
        }

        # replace token placeholders with tags
        for (k in seq_len(length(raw.token.tags)))
            new.text <- sub(token.placeholders[k], raw.token.tags[k], new.text,
                            ignore.case = TRUE, perl = TRUE)

        # replace raw replacements placeholders with formatted tags
        for (i in seq_len(length(raw.repl)))
        {
            tag <- paste0("<span class='raw-replacement' title='Replaced with: ",
                          escapeQuotesForHTML(raw.repl[[i]]$replacement), "'>",
                          raw.repl[[i]]$replaced, "</span>")
            new.text <- sub(raw.repl.placeholders[i], tag, new.text)
        }

        orig.text[j] <- new.text

        # Transformed text
        for (k in 1:length(trans.tokens.j))
            if (!is.na(ind[k]))
                # Add formatting to transformed text
                trans.tokens[[j]][k] <- paste0("<span class=\"word",
                                               ind[k], "\">",
                                               trans.tokens[[j]][k],
                                               "</span>")
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
    suffix <- ifelse(grepl("[a-zA-Z0-9_]$", x, perl = TRUE), "\\E\\b", "\\E")
    return(paste0(prefix, x, suffix))
}

# Escape single and double quotes to be used in HTML attributes such as title
# which is used for tooltips
escapeQuotesForHTML <- function(x)
{
    x <- gsub("\"", "&#34;", x, fixed = TRUE)
    x <- gsub("'", "&#39;", x, fixed = TRUE)
    x
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
#' @importFrom knitr kable
addTextPanel <- function(raw.and.normalized.text, row.numbers,
                         variable.numbers, variable.names, cata)
{
    if (!is.null(variable.numbers) && !is.null(variable.names) && all(is.finite(variable.numbers)))
        variable.numbers <- sprintf("<span title=\"%s\">%d</span>", variable.names[variable.numbers], variable.numbers)
    t.rownames <- if (!is.null(row.numbers) || !is.null(variable.numbers)) cbind(variable.numbers, row.numbers)
                  else                                                     rownames(raw.and.normalized.text)

    cata("<div id=\"text-panel\">")

        t <- cbind(t.rownames, raw.and.normalized.text)
        tmp.col.names <- c("Case", "Raw text", "Normalized text")
        if (NCOL(t.rownames) == 2)
            tmp.col.names <- c("Var", tmp.col.names)

        names(t) <- tmp.col.names
        rownames(t) <- NULL
        cata(kable(t, align = c(rep("c", NCOL(t.rownames)), "l", "l"), format = "html",
                   escape = FALSE, table.attr = "class=\"text-analysis-table\""))

    cata("</div>") # end panel div
}

addNGramsPanel <- function(n.gram.frequencies, cata)
{
    t <- n.gram.frequencies
    names(t) <- c(paste0("Category (", nrow(n.gram.frequencies), ")"), "Frequency", "Variants")

    cata("<div id=\"ngrams-panel\">")
    cata(kable(t, align = c("l", "c", "c"), format = "html", escape = FALSE,
               table.attr = "class=\"text-analysis-table\""))
    cata("</div>") # end panel div
}

addTopPanel <- function(cata, colored.text, raw.and.normalized.text)
{
    cata("<details open=\"true\" class=\"details\">")
    cata("<summary class=\"summary\">Categories</summary>")

    cata("<div class=\"top-container\">")

    addNGramsPanel(colored.text$n.grams, cata)
    addTextPanel(colored.text$text,
                 raw.and.normalized.text[["Row Numbers"]],
                 raw.and.normalized.text[["Variable Numbers"]],
                 raw.and.normalized.text[["Variable Names"]],
                 cata)
    cata("</div>", fill = TRUE) # end top-container div

    cata("</details>")
}

addDiagnosticsPanel <- function(cata, diagnostics)
{
    cata("<details class=\"details\">")
    cata("<summary class=\"summary\">Diagnostics</summary>")

    cata("<div class=\"diagnostics-container\">")

    # For each replacement, show cases where raw text has been replaced
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Raw text replacements</summary>")
    rawTextReplacementDiagnostic(cata, diagnostics$raw.text.replacements)
    cata("</details>")

    # For each manual category, show cases
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Required categories</summary>")
    cata("</details>")

    # For each delimiter, show cases which contain the delimiter
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Delimiters</summary>")
    cata("</details>")

    # For each conditional delimiter, show cases with conditional delimiter
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Conditional delimiters</summary>")
    cata("</details>")

    # For each split, show cases with split
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Splits by known categories</summary>")
    cata("</details>")

    # For each replacement, show cases with replacements
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Category replacements</summary>")
    cata("</details>")

    # Spelling corrections, showing cases for each correction
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Spelling corrections</summary>")
    cata("</details>")

    # Categories that have been discarded, showing cases
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Discarded categories</summary>")
    cata("</details>")

    # Categories below minimum frequency, showing cases
    cata("<details class=\"details\">")
    cata("<summary class=\"summary sub-details\">Categories below minimum frequency</summary>")
    cata("</details>")

    cata("</div>", fill = TRUE) # end diagnostics-container div
    cata("</div>", fill = TRUE) # end bottom-container div

    cata("</details>")
}

rawTextReplacementDiagnostic <- function(cata, info)
{
    cata("<div class=\"diagnostics-group\">")

    for (elem in info)
    {
        cata("<div class=\"diagnostics-block\">")

        t <- matrix(elem$replacement)
        colnames(t) <- "Replacement"
        cata(kable(t, align = c("l"), format = "html",
                   escape = FALSE, table.attr = "class=\"diagnostics-table\""))

        t <- matrix(elem$to.be.replaced)
        colnames(t) <- "Replaced"
        cata(kable(t, align = c("l"), format = "html",
                   escape = FALSE, table.attr = "class=\"diagnostics-table\""))

        t <- cbind(elem$raw.text.var.num, elem$raw.text.case.num,
                   elem$raw.text)
        colnames(t) <- c("Var", "Case", "Raw text")
        cata(kable(t, align = c("c", "c", "l"), format = "html",
                   escape = FALSE, table.attr = "class=\"diagnostics-table\""))

        cata("</div>")
    }
    cata("</div>")
}

createWidgetFromFile <- function(tfile)
{
    html <- paste(readLines(tfile), collapse = "\n")
    rhtmlMetro::Box(html, text.as.html = TRUE)
}
