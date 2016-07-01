#' GetLabels
#'
#' Replaces a list of given names, with any underlying labels, if they exist in the data.
#' @param names The names of the variables \code{data}.
#' @param data A \code{\link{data.frame}}.
#' @return A \code{vector} of names.
#' @export
GetLabels <- function(names, data)
{
    a <- unlist(lapply(data, function(x) attr(x, "label")))
    if (length(a) > 0)
    {
        matches <- match(names(a), names)
        valid.matches <- !is.na(matches)
        if (sum(valid.matches) > 0)
            names[matches[valid.matches]] <- a[valid.matches]
    }
    names
}

