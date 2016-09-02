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



#' \code{TrimLeadingWhitepsace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the beginning of a string.
#' @param x A \code{\link{character}} that may contain whitecontaining text to be modified.
#' @return A \code{character}.
#' @export
TrimLeadingWhitepsace <- function (x)  sub("^\\s+", "", x)

#' \code{TrimTrailingWhitespace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the end of a string.
#' @param x A \code{\link{character}} that may contain whitecontaining text to be modified.
#' @return A \code{character}.
#' @export
TrimTrailingWhitespace <- function (x) sub("\\s+$", "", x)

#' \code{TrimTrailingWhitespace}
#'
#' Removes whitespace (e.g.,spaces, tab characters) from the beginning or end of a string.
#' @param x A \code{\link{character}} that may contain whitecontaining text to be modified.
#' @return A \code{character}.
#' @export
TrimWhitespace <- function (x) gsub("^\\s+|\\s+$", "", x)
