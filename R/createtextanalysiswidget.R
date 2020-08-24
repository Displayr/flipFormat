#' @title Display text analysis output as an \code{htmlwidget}
#'
#' @description Creates a \code{htmlwidget} summary of diagnostic information for a
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
#' @param category.examples Examples for each category, to be displayed in the
#'   tooltips.
#' @param footer Character; footer to show at the bottom of the output.
#' @param diagnostics A list containing diagnostic information to be shown in
#'   the diagnostic output.
#' @param details.expand String of the name of the detail to expand.
#' @return An \code{htmlwidget} containing tables showing the output from a
#'   text analysis.
#' @seealso \code{\link[rhtmlMetro]{Box}}
#' @export
CreateTextAnalysisWidget <- function(raw.and.normalized.text,
                                     n.gram.frequencies,
                                     token.substitutions,
                                     footer = "",
                                     diagnostics = NULL,
                                     details.expand = "Categories",
                                     category.examples = NULL)
{
    # ptm2 <- proc.time()
    raw.and.normalized.text <- replaceMissingWithEmpty(raw.and.normalized.text)

    tfile <- createTempFile()
    cata <- createCata(tfile)

    addCss("table.css", cata)
    addCss("details.css", cata)
    addCss("textanalysis.css", cata)

    stylefile <- createTempFile()
    ws <- createCata(stylefile)

    # ptm <- proc.time()
    colored.text <- HighlightNGrams(n.gram.frequencies, raw.and.normalized.text,
                                    token.substitutions, category.examples,
                                    diagnostics$split.into.categories, ws)
    # print("highligh ngrams")
    # print(proc.time() - ptm)
    if (NROW(n.gram.frequencies) > 0)
        addCss(stylefile, cata, in.css.folder = FALSE)

    cata("<div class=\"main-container\">")

    show.diagnostics <- !is.null(diagnostics)

    addTopPanel(cata, colored.text, raw.and.normalized.text, show.diagnostics,
                details.expand)

    if (!is.null(diagnostics))
        addDiagnosticsPanel(cata, diagnostics, details.expand)

    addFooter(footer, cata)

    cata("</div>") # end main-container div

    output <- createWidgetFromFile(tfile)
    # print("CreateTextAnalysisWidget")
    # print(proc.time() - ptm2)
    output
}

replaceMissingWithEmpty <- function(raw.and.normalized.text)
{
    original.text <- raw.and.normalized.text[["Original Text"]]
    original.text[is.na(original.text)] <- ""
    raw.and.normalized.text[["Original Text"]] <- original.text
    raw.and.normalized.text
}

#' @importFrom stringi stri_rand_strings
#' @importFrom grDevices rgb col2rgb grey
#' @importFrom colorspace lighten darken
#' @importFrom flipU UniquePlaceholders EscapeRegexSymbols
HighlightNGrams <- function(n.grams, text, subs, category.examples,
                            split.into.categories, cata)
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
    cc <- c(); bb <- c()
    bbi <- 0
    for (i in 0:(n.rep-1))
    {
        if (i > 0 && (i %% n.col) == 0)
            bbi <- bbi + 1
        bb <- c(bb, paste0("border: 2px ", bcol[n.col - (i%%n.col)], "; ",
                           bshape[2 - (bbi%%2)], "border-style: ", bstyle[9 - (bbi%%9)], "; "))
    }
    border.styles <- rep(bb, each = length(col0))[1:n]
    ngram.seq <- seq(nrow(n.grams))
    base.seq <- vapply(ngram.seq, decimalToBase, character(1))
    # Create colours
    potential.colours <- setAlpha(col0, 0.5)
    # Make mapping for the colour index
    color.index <- 1:n %% n.col
    # Replace the 0 remainder with length of number of colours
    color.index[color.index == 0] <- n.col
    # Remove Unclassified tokens since they get separate styling
    unclassified <- which(n.grams[[1]] == "UNCLASSIFIED")
    if (length(unclassified))
    {
        ngram.seq <- ngram.seq[-unclassified]
        color.index <- color.index[-unclassified]
        border.styles <- border.styles[-unclassified]
    }
    color.indices <- split(ngram.seq, potential.colours[color.index])
    border.style.indices <- split(ngram.seq, border.styles)
    border.style.classes <- vapply(names(border.style.indices),
                                   function(x) paste0(paste0(".s", base.seq[border.style.indices[[x]]], collapse = ","),
                                                      "{", x, "}", collapse = ""),
                                   character(1), USE.NAMES = FALSE)

    color.classes <- vapply(names(color.indices),
                            function(x) paste0(paste0(".s", base.seq[color.indices[[x]]], collapse = ","),
                                               "{ background-color:", x, "}", collapse = ""),
                            character(1), USE.NAMES = FALSE)
    # Add styling (border styles, colours) to the file
    cata(color.classes, border.style.classes,
         paste0('[class^=s] { white-space: pre-wrap; line-height: 1.8em;}')) # Common styling
    # Inspect n.grams
    n.grams[[1]] <- as.character(n.grams[[1]])
    n.grams <- data.frame(n.grams, num.var = rep(1, n))
    orig.text <- text[["Original Text"]]
    trans.tokens <- text[["Transformed Text"]]
    patt <- n.grams[[1]]
    tooltips <- n.grams[[1]]

    n.gram.content <- n.grams[[1]]
    # Styling for unclassified tokens
    if (length(unclassified))
    {
        cata(paste0(paste0(".s", base.seq[unclassified], collapse = ","),
                    "{ background-color: #CCCCCC;}"))
        cata(paste0(paste0(".w", base.seq[unclassified], collapse = ", "),
                    ":after{content: \"UNCLASSIFIED\";}\n"))
        n.gram.content <- n.gram.content[-unclassified]
    }

    # Append the tokens to the span silently
    cata(paste0(".w", base.seq[ngram.seq], ":after{content: \"", n.gram.content, "\"}"))
    for (i in seq_len(n))
    {
        # Create regex for replacement
        replace.ind <- which(subs[, 2] == n.grams[i, 1])
        tmp.subs <- unique(subs[replace.ind, 1]) # group different capitalizations counted separately
        n.grams[i,3] <- length(tmp.subs)
        tmp.subs <- tmp.subs[!tmp.subs == n.grams[i, 1]]
        if (length(tmp.subs))
            tmp.subs <- paste0(length(tmp.subs),
                               ngettext(length(tmp.subs), " Variant: ", " Variants: "),
                               paste0(escapeQuotesForHTML(tmp.subs), collapse = ", "), "\n")
        tooltips[i] <- if (is.null(category.examples))
            paste0(escapeQuotesForHTML(tmp.subs), collapse = ", ")
        else
            paste0(tmp.subs,
                   paste(length(category.examples[[i]]), ngettext(length(category.examples[[i]]), "Example:\n", "Examples:\n"), collapse = " "),
                   paste0(escapeQuotesForHTML(category.examples[[i]]), collapse = "\n"))

        if (length(replace.ind) == 1)
            patt[i] <- paste0("(", escWord(subs[replace.ind,1]), ")")
        else if (length(replace.ind) > 1)
        {
            replace.ind <- replace.ind[order(nchar(subs[replace.ind,1]), decreasing = TRUE)]
            patt[i] <- paste0("(", paste(escWord(subs[replace.ind,1]), sep="", collapse="|"), ")")
            if (nchar(patt[i]) > 20000)
            {
                # if the pattern is too long (> 20K characters), we truncate the pattern
                # this may lead to some patterns being not highlighted
                tmp.nchar <- nchar(subs[replace.ind,1])
                tmp.cumsum <- cumsum(tmp.nchar)
                tmp.ind <- which(tmp.cumsum <= 20000)
                replace.ind <- replace.ind[tmp.ind]
                patt[i] <- paste0("(", paste(escWord(subs[replace.ind,1]), sep="", collapse="|"), ")")
            }
        }
        else
            patt[i] <- escWord(patt[i])
    }

    raw.replacement.rows <- sapply(text$`Raw replacement info`,
                                   function(x) x$row.index)
    split.categories.info <- text$`Split categories info`
    ind <- order(sapply(split.categories.info,
                        function(x) nchar(x$to.be.split)), decreasing = TRUE)
    split.categories.info <- split.categories.info[ind]
    split.text.placeholders <- UniquePlaceholders(length(split.categories.info))

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

            new.text <- substr(orig.text[j], 1, start.ind[1] - 1)
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

        # replace split text with placeholders
        split.ind <- if (!is.null(split.categories.info) &&
                         length(split.categories.info) > 0)
            which(sapply(split.categories.info, function(x) j %in% x$rows))
        else
            integer(0)

        split.text <- list()
        for (i in split.ind)
        {
            to.be.split.patt <- paste0("(?i)(?<!\\w)",
                EscapeRegexSymbols(split.categories.info[[i]]$to.be.split),
                "(?!\\w)")
            m <- gregexpr(to.be.split.patt, new.text, perl = TRUE)[[1]]
            split.text[[i]] <- sapply(seq(m), function(x) {
                    substr(new.text, m[x], m[x] + attr(m, "match.length")[x] - 1)
                })

            new.text <- gsub(to.be.split.patt, split.text.placeholders[i],
                             new.text, perl = TRUE)
        }

        # replace tokens with placeholders
        n.tokens <- length(trans.tokens.j)
        raw.token.tags <- character(0)
        token.placeholders <- paste0("-", make.unique(stri_rand_strings(n.tokens, 64)), "-")
        found <- logical(n.tokens)
        for (k in 1:n.tokens)
        {
            if (!is.na(ind[k]))
            {
                mpos <- regexpr(patt[ind[k]], new.text,
                                ignore.case = TRUE, perl = TRUE)
                if (mpos != -1)
                {
                    found[k] <- TRUE
                    raw.token <- substr(new.text, mpos,
                                        mpos + attr(mpos, "match.length") - 1)
                    # If raw token same as n.grams token get the content from the .wXXXX css class
                    if (raw.token %in% n.grams[[1]])
                        tag <- paste0("<a class=\"s", base.seq[ind[k]], " w", base.seq[ind[k]], "\">", "</a>")
                    else # Otherwise if the raw token is not the same, keep the raw token
                        tag <- paste0("<a class=\"s", base.seq[ind[k]], "\">", htmlText(raw.token), "</a>")
                    raw.token.tags <- c(raw.token.tags, tag)
                    new.text <- sub(patt[ind[k]], token.placeholders[k],
                                    new.text, ignore.case = TRUE, perl = TRUE)
                }
            }
        }
        token.placeholders <- token.placeholders[found]

        # replace token placeholders with tags
        for (k in seq(raw.token.tags))
            new.text <- sub(token.placeholders[k], raw.token.tags[k], new.text,
                            ignore.case = TRUE, perl = TRUE)

        # replace raw replacements placeholders with formatted tags
        for (i in seq(raw.repl))
        {
            tag <- paste0("<span class='raw-replacement' title='Replaced with: ",
                          escapeQuotesForHTML(raw.repl[[i]]$replacement), "'>",
                          htmlText(raw.repl[[i]]$replaced), "</span>")
            new.text <- gsub(raw.repl.placeholders[i], tag, new.text)
        }

        # replace split text placeholders with formatted tags
        for (i in split.ind)
        {
            categories <- paste0(escapeQuotesForHTML(split.categories.info[[i]]$categories),
                                collapse = ", ")
            for (t in split.text[[i]])
            {
                tag <- paste0("<span class='split-text' title='Split into: ",
                              categories, "'>", htmlText(t), "</span>")
                new.text <- sub(split.text.placeholders[i], tag, new.text)
            }
        }

        orig.text[j] <- new.text

        # Transformed text
        for (k in 1:length(trans.tokens.j))
            if (!is.na(ind[k]))
                # Add formatting to transformed text
                trans.tokens[[j]][k] <- paste0("<a class=\"s", base.seq[ind[k]], " w", base.seq[ind[k]], "\">",
                                               # htmlText(trans.tokens[[j]][k]),
                                               "</a>")
    }

    trans.text <- sapply(trans.tokens, paste, collapse = " ")

    # Create n-grams table with number of counts and variants
    # Tooltips is added via "title" - not related to the class CSS
    if (nrow(n.grams) > 0)
        n.grams[,1] <- paste0("<a class=\"s", base.seq[1:n], " w", base.seq[1:n], "\" title=\"",
                              tooltips, "\">",  "</a>") #htmlText(n.grams[,1]),

    # Replace any newline characters with <br>
    orig.text <- gsub("\r\n|\n\r|\n|\r", "<br>", orig.text)

    # Prevent incomplete final line warning
    cata("\n")
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

    cata("<div class=\"panel text-panel\">")

    t <- cbind(t.rownames, raw.and.normalized.text)
    tmp.col.names <- c("Case", "Raw text", "Normalized text")
    if (NCOL(t.rownames) == 2)
        tmp.col.names <- c("Var", tmp.col.names)

    names(t) <- tmp.col.names
    rownames(t) <- NULL

    cata(kable(t, align = NULL, format = "html",
               escape = FALSE, table.attr = "class=\"text-analysis-table raw-text-table\""))

    cata("</div>") # end panel div
}

addNGramsPanel <- function(n.gram.frequencies, cata)
{
    t <- n.gram.frequencies
    names(t) <- c(paste0("Category (", nrow(n.gram.frequencies), ")"), "Frequency", "Variants")

    cata("<div class=\"panel ngrams-panel\">")
    cata(kable(t, align = NULL, format = "html", escape = FALSE,
               table.attr = "class=\"text-analysis-table categories-table\""))
    cata("</div>") # end panel div
}

addTopPanel <- function(cata, colored.text, raw.and.normalized.text,
                        show.diagnostics, details.expand)
{
    if (show.diagnostics)
    {
        if (details.expand == "Categories")
            cata("<details open=\"true\" class=\"details\">")
        else
            cata("<details class=\"details\">")
        cata("<summary class=\"summary\">Categories</summary>")
        cata("</details>")
    }

    cata("</details>")
    cata("<div class=\"panel-container\">")

    addNGramsPanel(colored.text$n.grams, cata)
    addTextPanel(colored.text$text,
                 raw.and.normalized.text[["Row Numbers"]],
                 raw.and.normalized.text[["Variable Numbers"]],
                 raw.and.normalized.text[["Variable Names"]],
                 cata)

    cata("</div>")
}

addDiagnosticsPanel <- function(cata, diagnostics, details.expand)
{
    html <- if (details.expand != "Categories")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html, "<summary class=\"summary\">Diagnostics</summary>",
                   "</details><div class=\"panel-container\"><div class=\"panel diagnostics-panel\">")

    html <- paste0(html, variantSuggestionsDiagnostic(diagnostics$variant.suggestions,
                                                      details.expand))

    # ptm <- proc.time()
    # For each replacement, show cases where raw text has been replaced
    if (length(diagnostics$raw.text.replacement) > 0)
        html <- paste0(html, rawTextReplacementDiagnostic(diagnostics$raw.text.replacement,
                                                          details.expand))
    # print("raw text replacement")
    # print(proc.time() - ptm)

    # ptm <- proc.time()
    # For each manual category, show cases
    html <- paste0(html, requiredCategoriesDiagnostic(diagnostics$required.categories,
                                                      details.expand))
    # print("required categories")
    # print(proc.time() - ptm)

    # ptm <- proc.time()
    # For each delimiter, show cases which contain the delimiter
    html <- paste0(html, delimitersDiagnostic(diagnostics$delimiters,
                                              details.expand))
    # print("delimiters")
    # print(proc.time() - ptm)

    # For each conditional delimiter, show cases with conditional delimiter
    html <- paste0(html, conditionalDelimitersDiagnostic(diagnostics$conditional.delimiters,
                                                         details.expand))

    # ptm <- proc.time()
    # For each split, show cases with split
    html <- paste0(html, knownCategoriesSplitDiagnostic(diagnostics$known.category.splits,
                                                        details.expand))
    # print("known category splits")
    # print(proc.time() - ptm)

    html <- paste0(html, splitIntoCategoriesDiagnostic(diagnostics$split.into.categories,
                                                       details.expand))

    # ptm <- proc.time()
    # For each replacement, show cases with replacements
    if (length(diagnostics$category.replacements) > 0)
        html <- paste0(html, categoryReplacementDiagnostic(diagnostics$category.replacements,
                                                           details.expand))
    # print("category replacements")
    # print(proc.time() - ptm)

    # ptm <- proc.time()
    # Spelling corrections, showing cases for each correction
    html <- paste0(html, spellingCorrectionsDiagnostic(diagnostics$spelling.corrections,
                                                       details.expand))
    # print("spelling corrections")
    # print(proc.time() - ptm)

    # ptm <- proc.time()
    # Categories that have been discarded, showing cases
    html <- paste0(html, discardedCategoriesDiagnostic(diagnostics$discarded.categories,
                                                       details.expand))
    # print("discarded")
    # print(proc.time() - ptm)

    # ptm <- proc.time()
    # Categories below minimum frequency, showing cases
    html <- paste0(html, lowFrequencyCategoriesDiagnostic(diagnostics$low.freq.categories,
                                                          details.expand))
    # print("min freq")
    # print(proc.time() - ptm)

    html <- paste0(html, "</div></div>") # end panel and panel-container divs

    cata(html)
}

variantSuggestionsDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand %in% c("Categories", "Variant suggestions"))
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Variant suggestions (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Suggestions for category variants are shown below. ",
                   "The suggestions can be copied and pasted into the variant ",
                   "columns in the REQUIRED CATEGORIES table editor.</div>")

    n.categories <- length(info)

    if (n.categories > 0)
    {
        max.variants <- max(sapply(info, length))
        t <- matrix("", nrow = n.categories, ncol = max.variants + 1)
        colnames(t) <- c("Category", paste0(rep("Variant ", max.variants), 1:max.variants))
        t[, 1] <- htmlText(names(info))
        for (i in seq_len(n.categories))
            t[i, 2:(length(info[[i]]) + 1)] <- htmlText(info[[i]])

        html <- paste0(html, "<div class=\"diagnostics-block\">")
        html <- paste0(html, kable(t, format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))
        html <- paste0(html, "</div>")
    }

    paste0(html, "</div></details>")
}

rawTextReplacementDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Raw text replacements")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Raw text replacements (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Raw text replacements are specified by clicking on ",
                   "the button under the RAW TEXT REPLACEMENT group and ",
                   "entering the replacement text in the first column and ",
                   "the text to be replaced in subsequent columns of the ",
                   "table editor. Raw text replacements are applied before ",
                   "all other text processing. This option can be ",
                   "used to split text that would not otherwise be split, ",
                   "e.g.: \"Coke Pepsi\" would not be split ",
                   "if spaces are not delimiters, but replacing it ",
                   "with \"Coke, Pepsi\" would cause it to be split (if ",
                   "commas are delimiters).</div>")

    for (elem in info)
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")

        t <- matrix("", nrow = length(elem$to.be.replaced), ncol = 2)
        t[, 1] <- htmlText(elem$to.be.replaced)
        t[1, 2] <- htmlText(elem$replacement)
        colnames(t) <- c("Replaced", "Replacement")
        html <- paste0(html, kable(t, align = c("l", "l"), format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(elem))

        html <- paste0(html, "</div>")
    }
    paste0(html, "</div></details>")
}

requiredCategoriesDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Required categories")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Required categories (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Required categories are specified by clicking on ",
                   "the button under the REQUIRED CATEGORIES group and ",
                   "entering the required categories in the first column. ",
                   "Variants of the required categories are specified in ",
                   "subsequent columns. This allows the consolidation of ",
                   "different variants of a category. ",
                   "A required category will always appear in the list of ",
                   "categories and not be split into smaller categories, ",
                   "spell corrected or removed if they fall below the minimum ",
                   "category frequency.</div>")

    for (elem in info)
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")

        if (length(elem$variants) > 0)
        {
            t <- matrix("", ncol = 2, nrow = length(elem$variants))
            t[1, 1] <- htmlText(elem$required.category)
            t[, 2] <- htmlText(elem$variants)
            colnames(t) <- c("Required", "Variants")
        }
        else
        {
            t <- matrix(htmlText(elem$required.category))
            colnames(t) <- c("Required")
        }
        html <- paste0(html, kable(t, format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(elem))

        html <- paste0(html, "</div>")
    }
    paste(html, "</div></details>")
}

delimitersDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Delimiters")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Delimiters (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Delimiters are used to split raw text into categories. ",
                   "Delimiters are selected in the DELIMITERS / SPLIT TEXT group. ",
                   "If a required delimiter is not listed in the checkboxes, ",
                   "it should be added to the textbox labeled \"Other\".</div>")

    for (elem in info)
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")

        t <- matrix(htmlText(elem$delimiter))
        colnames(t) <- "Delimiter"
        html <- paste0(html, kable(t, align = c("l"), format = "html",
                   escape = FALSE, table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(elem))

        html <- paste0(html, "</div>")
    }
    paste0(html, "</div></details>")
}

conditionalDelimitersDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Conditional delimiters")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Conditional delimiters (",
                   length(info$conditional.delimiters), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Conditional delimiters are only used to split text if the ",
                   "resulting splits exist as categories. ",
                   "Conditional delimiters are specified in textbox labeled \"Conditional\" ",
                   "in the DELIMITERS / SPLIT TEXT group.</div>")
    if (!is.null(info))
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")
        t <- matrix(htmlText(info$conditional.delimiters))
        colnames(t) <- "Delimiters"
        html <- paste0(html, kable(t, align = c("l"), format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(info))
        html <- paste0(html, "</div>")
    }
    paste0(html, "</div></details>")
}

knownCategoriesSplitDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Splits by known categories")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Splits by known categories (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Categories with frequencies at or below the \"Maximum ",
                   "category frequency to split\" setting in DELIMITERS / ",
                   "SPLIT TEXT are split by known categories, which are ",
                   "defined by the \"Minimum known category frequency\" ",
                   "setting. To prevent a category from being split, specify ",
                   "it as a required category.</div>")
    for (elem in info)
    {

        html <- paste0(html, "<div class=\"diagnostics-block\">")
        ptm <- proc.time()

        replacements <- htmlText(elem$replacements)
        replacements[elem$is.known.category] <- paste0("<span style='font-weight:bold'>",
                                                       replacements[elem$is.known.category],
                                                       "</span>")

        t <- matrix("", nrow = length(elem$replacements), ncol = 2)
        t[1, 1] <- htmlText(elem$replaced)
        t[, 2] <- replacements
        colnames(t) <- c("Split text", "Splits")
        html <- paste0(html, kable(t, align = c("l", "l"), format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(elem))

        html <- paste0(html, "</div>")

    }
    paste0(html, "</div></details>")
}

splitIntoCategoriesDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Splits into categories")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Splits into categories (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Phrases can be manually split into categories by ",
                   "clicking on the button at the bottom of the ",
                   "DELIMITERS / SPLIT TEXT group, specifying the phrase to ",
                   "be split in the first column and specifying the ",
                   "resulting categories from the split in the subsequent ",
                   "columns. This feature is used when categories cannot be ",
                   "split by delimiters.</div>")

    for (elem in info)
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")

        t <- matrix("", nrow = length(elem$categories), ncol = 2)
        t[1, 1] <- htmlText(elem$to.be.split)
        t[, 2] <- htmlText(elem$categories)
        colnames(t) <- c("Split phrase", "Categories from split")
        html <- paste0(html, kable(t, align = c("l", "l"), format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(elem))

        html <- paste0(html, "</div>")
    }
    paste0(html, "</div></details>")
}

categoryReplacementDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Category replacements")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Category replacements (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Category replacements are specified by clicking on ",
                   "the button under the CATEGORY REPLACEMENT group and ",
                   "entering the replacement category in the first column and ",
                   "the categories to be replaced in subsequent columns of the ",
                   "table editor. This option is used to combine categories ",
                   "that should be the same.</div>")

    for (elem in info)
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")

        t <- matrix("", nrow = length(elem$to.be.replaced), ncol = 2)
        t[, 1] <- htmlText(elem$to.be.replaced)
        t[1, 2] <- htmlText(elem$replacement)
        colnames(t) <- c("Replaced", "Replacement")
        html <- paste0(html, kable(t, align = c("l", "l"), format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(elem))

        html <- paste0(html, "</div>")
    }
    paste0(html, "</div></details>")
}

spellingCorrectionsDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Spelling corrections")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Spelling corrections (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Spelling correction settings are found in the ",
                   "SPELLING CORRECTION group. To prevent a pharse from being ",
                   "corrected, click on the \"Phrases that shouldn't be corrected\" ",
                   "button and enter the phrase in the first column of the ",
                   "table editor.</div>")

    for (elem in info)
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")

        t <- matrix("", nrow = 1, ncol = 2)
        t[1, 1] <- htmlText(elem$corrected)
        t[1, 2] <- htmlText(elem$correction)
        colnames(t) <- c("Corrected", "Correction")
        html <- paste0(html, kable(t, align = c("l", "l"), format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(elem))

        html <- paste0(html, "</div>")
    }
    paste0(html, "</div></details>")
}

discardedCategoriesDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Discarded categories")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Discarded categories (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Categories to be discarded can be specified by clicking on ",
                   "the button under the CATEGORIES TO DISCARD group and ",
                   "entering the categories in the first column of the table ",
                   "editor. Discarded categories will be removed but are still ",
                   "counted in the UNCLASSIFIED category.</div>")

    for (elem in info)
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")

        t <- matrix(htmlText(elem$discarded))
        colnames(t) <- "Discarded"
        html <- paste0(html, kable(t, align = c("l"), format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table\""))

        html <- paste0(html, rawCasesTable(elem))

        html <- paste0(html, "</div>")
    }
    paste0(html, "</div></details>")
}

lowFrequencyCategoriesDiagnostic <- function(info, details.expand)
{
    html <- if (details.expand == "Categories below minimum frequency")
        "<details open=\"true\" class=\"details\">"
    else
        "<details class=\"details\">"

    html <- paste0(html,
                   "<summary class=\"summary sub-details\">Categories below minimum frequency (",
                   length(info), ")</summary>",
                   "<div class=\"diagnostics-group\">",
                   "<div class=\"diagnostics-message\">",
                   "Categories below the minimum category size are shown below. ",
                   "These categories are removed but are still counted in the ",
                   "UNCLASSIFIED category. The minimum category size option ",
                   "appears in the DATA SOURCE group.</div>")

    if (length(info) > 0)
    {
        html <- paste0(html, "<div class=\"diagnostics-block\">")

        n.row <- sum(sapply(info, function(x) length(x$raw.text)))

        t <- matrix("", nrow = n.row, ncol = 4)
        colnames(t) <- c("Discarded", "Var", "Case", "Raw text")

        ind <- 1
        for (elem in info)
        {
            n.raw.text <- length(elem$raw.text)
            ind.raw.text <- ind:(ind + n.raw.text - 1)
            t[ind, 1] <- elem$low.freq.category
            t[ind.raw.text, 2] <- elem$raw.text.var.num
            t[ind.raw.text, 3] <- elem$raw.text.case.num
            t[ind.raw.text, 4] <- elem$raw.text
            ind <- ind + n.raw.text
        }

        html <- paste0(html, kable(t, align = c("l", "c", "c", "l"),
                                   format = "html", escape = FALSE,
                                   table.attr = "class=\"diagnostics-table low-freq-table\""))

        html <- paste0(html, "</div>")
    }
    paste0(html, "</div></details>")
}

# Create table to display raw cases. obj contains the raw text along with the
# corresponding variable numbers and case numbers.
rawCasesTable <- function(obj)
{
    html <- "<div class=\"diagnostics-raw-cases\">"
    if (length(obj$raw.text) > 0)
    {
        t <- cbind(obj$raw.text.var.num, obj$raw.text.case.num,
                   htmlText(obj$raw.text))
        if (obj$is.max.exceeded)
            t <- rbind(t, c("", "", htmlText(paste0("<TABLE TRUNCATED. ",
                                                      obj$n.omitted.rows,
                                                      " ROWS OMITTED>"))))

        colnames(t) <- c("Var", "Case", "Raw text")
        html <- paste0(html, kable(t, align = c("c", "c", "l"),
                                   format = "html", escape = FALSE,
                                   table.attr = "class=\"diagnostics-table raw-cases-table\""))
    }
    else
    {
        t <- matrix(htmlText("<NO CASES FOUND>"))
        colnames(t) <- "Raw text"
        html <- paste0(html, kable(t, align = c("l"), format = "html",
                                   escape = FALSE,
                                   table.attr = "class=\"diagnostics-table raw-cases-table\""))
    }
    paste0(html, "</div>")
}

addFooter <- function(footer, cata)
{
    cata("<div class=\"footer-container\"><div class=\"footer-spacing\"><div class=\"footer\">",
         footer, "</div></div></div>")
}

htmlText <- function(html)
{
    gsub("\r\n|\n\r|\n|\r", "<br>", htmlEscape(html))
}

createWidgetFromFile <- function(tfile)
{
    # This is required to stop the "incomplete final line" warning from
    # readLines
    cat("", fill = TRUE, file = tfile, append = TRUE)

    html <- paste(readLines(tfile), collapse = "\n")
    rhtmlMetro::Box(html, text.as.html = TRUE)
}

#' Extract the transformed text from the transformed tokens.
#'
#' Convert the transformed tokenized element to transformed text. Adjusting for the blanks after
#' transformation as necessary.
#' @param x Either a object of class \code{wordBag} or a list with a
#' \code{transformed.text} element. In the former, the transformed text is created  from the transformed
#' tokenized list element. If the latter then the \code{transformed.text} element is returned.
#' @return A character vector with the transformed text determined from the list of tokens
#' @export
CreateTransformedText <- function(x)
{
    # For legacy outputs or other outputs that already have the element
    # then return word.bags
    if (!is.null(x$transformed.text))
        return(x$transformed.text)
    if (!inherits(x, "wordBag"))
        stop("Transformed text can only be created from an object of class: wordBag")
    subset <- x$subset
    text <- tolower(x$original.text)
    n.subset <- sum(subset)
    n.cases <- length(text)
    # Convert the transformed tokenized to a list of characters
    if (n.subset < n.cases)
    {
        transformed.tokenized <- vector("list", length = n.cases)
        transformed.tokenized[subset] <- decodeNumericText(x$transformed.tokenized[subset],
                                                           levels = attr(x$transformed.tokenized, "levels"))
    } else
        transformed.tokenized <- decodeNumericText(x$transformed.tokenized)
    # Determine if there are any cases with blank entries after transformation
    blank.after.transform <- x$blank.after.transform
    transformed.text <- vapply(transformed.tokenized, paste, character(1),
                               collapse = " ")

    if (n.subset < length(text))
    {
        transformed.text[!subset] <- text[!subset]
        transformed.text[subset][blank.after.transform] <- "<NO_WORDS_REMAIN_AFTER_PROCESSING>"
    } else
        transformed.text[blank.after.transform] <- "<NO_WORDS_REMAIN_AFTER_PROCESSING>"

    transformed.text
}

#' Function to convert the list of numerically encoded text back to a list of character vectors
#' (or as a character vector from a factor). Typical use case is a list of integer vectors to represent
#' different text elements. The integers are mapped to the text elements using a levels attribute.
#' If the levels attribute is not found, then a second argument is inspected. The second argument is useful when
#' a list has been subsetted and the attribute of the original list is lost.
#' @param tokens A list of integer vectors or a single factor
#' @param levels The character vector whose indices contain the characters to decode the tokens.
#' @noRd
decodeNumericText <- function(text, levels = NULL)
{
    if (inherits(text, "list"))
    {
        if (is.null(levels))
            levels <- attr(text, "levels")
        if (is.null(levels))
            return(text)
        text <- lapply(text, function(x) levels[x])
    } else if (inherits(text, "factor"))
        text <- as.character(text)
    else if (inherits(text, "character"))
        text
    else
        stop("Unexpected input: text needs to be a character vector, list or a factor")
    text
}
