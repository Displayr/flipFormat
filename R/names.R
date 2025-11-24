#' \code{Names}
#'
#' Similar to \link{name}, except returns \code{attr(x, "name")} or \link{OriginalName} with a vector, if they exist.
#' @param x A variable or a \link{data.frame}.
#' @export
Names <- function(x)
{
    if (is.list(x))
    {
        nms <- names(x)
        for (i in seq_along(x))
        {
            name.attribute <- attr(x[[i]], "name")
            if (!is.null(name.attribute))
                nms[i] <- name.attribute

        }
        return(nms)
    }
    name.attribute <- attr(x, "name")
    if (!is.null(name.attribute))
        return(name.attribute)
    OriginalName(x)
}



#' \code{RemoveParentName}
#'
#' Replaces a list of given names, with any underlying labels, if they exist in the data.
#' @param x A \code{\link{character}}, typically containing a variable name.
#' @return A \code{character}.
#' @details Strips out everything up to and including the last $. E.g., replaces
#' \code{"foo$fog$x"} with \code{"x"}
#' @examples
#' RemoveParentName("foo$fog$x")
#' @export
RemoveParentName <- function(x)
{
    ReplacingEverythingBefore(x, "\\$")
}


#' \code{OriginalName}
#'
#' Finds the original name of an argument.
#' @param x An object of some kind.
#' @return A \code{character} of the name.
#' @details This function will not generate the correct answer if the actual call is nested within another function.
#' @references Adapted from http://stackoverflow.com/a/26558733/1547926.
#' @importFrom utils head
#' @export
OriginalName <- function(x)
{
    my.call <- quote(substitute(x))
    original.name <- eval(my.call)
    for(i in rev(head(sys.frames(), -1L)))
    {
        prev <- original.name
        my.call[[2]] <- original.name
        original.name <- eval(my.call, i)
        if (length(original.name) == length(x) & length(prev) == 1)
            return(prev)
    }
    original.name <- paste(original.name)
    if (length(original.name) > 2)
        return(paste0(c(original.name[2:1], original.name[-1:-2]), collapse = ""))
    return(paste(original.name, collapse = ""))
}
