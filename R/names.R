#' \code{RemoveParentName}
#'
#' Replaces a list of given names, with any underlying labels, if they exist in the data.
#' @param x A \code{\link{character}}, typically containing a variable name.
#' @return A \code{character}.
#' @details Strips out everything up to and including the last \$. E.g., replaces
#' \code{"foo$fog$x"} with \code{"x"}
#' @examples
#' RemoveParentName("foo$fog$x")
#' @export
RemoveParentName <- function(x)
{
    ReplacingEverythingBefore(x, "\\$")
}

