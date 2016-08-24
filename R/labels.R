#' \code{GetLabels}
#'
#' Replaces a list of given names, with any underlying labels, if they exist in the data.
#' @param names The names of the variables \code{data}.
#' @param data A \code{\link{data.frame}}.
#' @return A \code{vector} of names.
#' @export
GetLabels <- function(names, data)
{
    list.of.labels <- lapply(data, function(x) attr(x, "label"))
    labels <- unlist(list.of.labels)
    k <- ncol(data)
    levels <- lapply(data, levels)
    nlevels <- sapply(data, nlevels)
    factors <- sapply(data, is.factor)
    variable.names <- names(data)
    if (length(labels) > 0)
    {
        # Fixing numeric labels.
        matches <- match(names(labels), names)
        valid.matches <- !is.na(matches)
        if (sum(valid.matches) > 0)
            names[matches[valid.matches]] <- labels[valid.matches]
        # Fixing factor labels.
        for (i in 1:k)
        {
            label <- list.of.labels[[i]]
            if (factors[i] & !is.null(label))
            {
                levs <- levels[[i]][-1]
                old.labels <- paste0(variable.names[i], levs)
                new.labels <- paste0(label, ": ", levs)
                names[match(old.labels, names)] <- new.labels
            }

        }
    }
    names
}

