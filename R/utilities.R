
#' \code{RemoveParentName}
#'
#' Strips out everything up to and perhaps including a specific character.
#' @param x A \code{\link{character}} containing text to be modified.
#' @param marker A \code{character} containing the things to be searched for. You need to use
#' backslashes if not matching for letters (e.g., a \code{"\\$"}. Where there are mutliple instances, the last is found.
#' @param include.marker Whether or not the \code{marker} itself is to be replaced.
#' @return A \code{character}.
#' @examples
#' ReplacingEverythingBefore("foo$fog$x", "\\$")
#' ReplacingEverythingBefore("foo$fog$x", "\\$", FALSE)
#' ReplacingEverythingBefore("GiraffeDogCat", "Dog", FALSE)
#' ReplacingEverythingBefore("GiraffeDogCat", "Dog", TRUE)

#' @export
ReplacingEverythingBefore <- function(x, marker, include.marker = TRUE)
{
    n.x <- length(x)
    if (n.x > 1)
    {
        result <- NULL
        for (x.part in x)
            result <- c(result, ReplacingEverythingBefore(x.part, marker, include.marker))
        return(result)
    }
    r <- gregexpr(marker, x)[[1]]
    position <- max(r)
    if (position == -1)
        return(x)
    if (include.marker)
    {
        len <- attr(r, "match.length")
        position <- position + len[length(len)]
    }
    substring(x, position)
}

#' \code{RemoveParentName}
#'



#' \code{ExtractCommonPrefix}
#'
#' Returns the common prefix of a vector of labels.
#' @param labels A vector of labels from which we plan to extract a common prefix.
#' @return A list with components
#' \itemize{
#' \item \code{common.prefix} - The common prefix shared among the labels,
#' or \code{NA} if none exists
#' \item \code{shortened.labels} - vector of labels with the common prefix removed.
#' }
#' @export
ExtractCommonPrefix <- function(labels)
{
    separators <- c(" ", ":", ",", ";", "-")
    last.prefix.index <- NA # this will be last index of the prefix
    n.labels <- length(labels)
    if (n.labels > 1)
    {
        inside.word <- FALSE
        min.length <- min(nchar(labels))
        for (i in 1:min.length)
        {
            if (characterMatches(labels, i))
            {
                ch <- substr(labels[1], i, i)
                if (ch %in% separators)
                {
                    if (inside.word && i < min.length && hasTextFromIndexOnwards(labels, i + 1, separators))
                        last.prefix.index <- i - 1
                    inside.word <- FALSE
                }
                else
                    inside.word <- TRUE
            }
            else
                break
        }
    }

    if (!is.na(last.prefix.index))
    {
        shortened.labels <- substr(labels, last.prefix.index + 1, 1000)
        shortened.labels <- gsub(paste0("^[", paste0(separators, collapse = "") , "]+"), "", shortened.labels)
        list(common.prefix = substr(labels[1], 1, last.prefix.index),
             shortened.labels = shortened.labels)
    }
    else
        list(common.prefix = NA, shortened.labels = labels)
}

#' Extract shortened labels with common prefix removed
#'
#' Returns the common prefix of a vector of labels.
#' @param data data containing labels; see \code{\link{Labels}}
#' @param tidy logical; if \code{TRUE} the returned labels are also trimmed of whitespace
#' and have first letter capitalized
#' @return A list with components
#' \itemize{
#' \item \code{common.prefix} - The common prefix shared among the labels,
#' or \code{NA} if none exists
#' \item \code{shortened.labels} - vector of labels with the common prefix removed.
#' }
#' @seealso \code{\link{ExtractCommonPrefix}}, \code{\link{Labels}}, \code{\link{TidyLabels}}
#' @export
ExtractCommonPrefixFromLabels <- function(data, tidy = TRUE){
    out <- ExtractCommonPrefix(Labels(data))
    if (tidy)
        out$shortened.labels <- TidyLabels(out$shortened.labels)
    out
}


#' \code{TidyLabels}
#'
#' Takes a vector of labels, removes any common prefix,  capitalizes the first letter, removes whitespace from the end.
#' @param labels A vector of labels from which we plan to extract a common prefix.
#' @seealso \code{\link{ExtractCommonPrefix}}
#' @return A vector of \code{character}s.
#' @importFrom flipU TrimTrailingWhitespace
#' @export
TidyLabels <- function(labels)
{
    if (length(labels) == 1)
        return(labels)
    # Removing any common prefix
    labels <- ExtractCommonPrefix(labels)$shortened.labels
    # Removing whitepace from the end
    labels <- TrimTrailingWhitespace(labels)
    # Capitalizing the first label
    first.letter <- toupper(substr(labels, 1, 1))
    ifelse(nchar(labels) > 1, paste0(first.letter, substr(labels, 2, nchar(labels))), first.letter)
}



# Check that character at an index is the same for all labels.
characterMatches <- function(labels, index)
{
    ch <- substr(labels[1], index, index)
    is.matching <- TRUE
    for (j in 2:length(labels))
        if (ch != substr(labels[j], index, index))
        {
            is.matching <- FALSE
            break
        }
    is.matching
}

# Check that a string x only consists of characters chars.
consistsOf <- function(x, chars)
{
    result <- TRUE
    for (i in 1:nchar(x))
        if (!(substr(x, i, i) %in% chars))
        {
            result <- FALSE
            break
        }
    result
}

# Check that there are word characters from an index onwards
hasTextFromIndexOnwards <- function(labels, index, non.word.chars)
{
    result <- TRUE
    for (i in 1:length(labels))
    {
        if (consistsOf(substr(labels[i], index, nchar(labels[i])), non.word.chars))
        {
            result <- FALSE
            break;
        }
    }
    result
}

possible.chars <- c(0:9, letters, "-", "_")
# Convert index to higher counting base for character efficiency in class names
# valid css identifier name seem to be able to use base 64 (0-9,a-z,A-Z,- and _)
# see for example the first bullet point at https://www.w3.org/TR/CSS21/syndata.html#characters
# However, HTML class names seem to be case insensitive (so "wA" would be the same as "wa")
#' @param x Integer scalar to be converted to base 38
#' @noRd
decimalToBase <- function(x, base = 38)
{
    out <- ""
    while (x)
    {
        r <- x %% base + 1
        x <- x %/% base
        out <- paste0(possible.chars[r], out)
    }
    out
}


generateRandomString <- function(digits = 10)
{
    paste0(sample(possible.chars, digits, replace = TRUE), collapse = "")
}

#' @importFrom rhtmlMetro Box
boxIframeless <- function(...)
{
    w <- Box(...)
    attr(w, "can-run-in-root-dom") <- TRUE
    w
}
