#' @importFrom httr GET content
checkImageTag <- function(text)
{
    patt <- "<img src=([\"'])((?:\\\\?+.)*?)\\1>"
    mm <- regexpr(patt, text, perl = TRUE)
    mlen <- attr(mm, "capture.length")
    mstart <- attr(mm, "capture.start")
    if (mm < 0 || length(mlen) < 2 || mlen[2] < 1)
    {
        warning("Table content contains an image tag with a syntax error which has been removed: ", text)
        return("")
    } else
    {
        imgtag <- substr(text, mm, mm + attr(mm, "match.length") - 1)
        imglink <- substr(text, mstart[2], mstart[2] + mlen[2] - 1)
        response <- try(GET(imglink), silent = TRUE)
        if (inherits(response, "try-error") || response$status_code != 200)
        {
            warning("Table content contains an image tag with an invalid link which has been removed: ", text)
            return(sub(imgtag, "", text, fixed = TRUE))
        } else
        {
            # if there is no problem with the image tag then enclose
            # inside a div to preserve alignment inside table
            return(paste0("<div>", text, "</div>"))
        }
    }
}
