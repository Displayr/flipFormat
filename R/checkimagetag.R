#' @importFrom httr GET content
checkImageTag <- function(text)
{
    mm <- regexpr("<img [^>]+>", text, perl = TRUE)
    if (mm < 0)
    {
        warning("Table content contains an image tag with a syntax error which has been removed: ", text)
        return("")
    }
    imgtag <- substr(text, mm, mm + attr(mm, "match.length") - 1)

    mm2 <- regexpr("src=(\\S+)", imgtag, perl = TRUE)
    if (mm2 < 0)
    {
        warning("Table content contains an image tag with a syntax error which has been removed: ", text)
        return("")
    }

    imglink <- substr(imgtag, attr(mm2, "capture.start")[1],
                      attr(mm2, "capture.start")[1] + attr(mm2, "capture.length") - 1)
    imglink <- sub("\\s+", "", imglink)
    imglink <- sub("^['\"]", "", imglink)
    imglink <- sub(">$", "", imglink)
    imglink <- sub("\\s+$", "", imglink)
    imglink <- sub("['\"]$", "", imglink)
    if (!nzchar(imglink))
    {
        warning("Table content contains an image tag with an empty link")
        return(sub(imgtag, "", text, fixed = TRUE))
    }

    response <- try(GET(imglink), silent = TRUE)
    if (inherits(response, "try-error") || response$status_code != 200)
    {
        warning("Table content contains an image tag with an invalid link which has been removed: ", imglink)
        return(sub(imgtag, "", text, fixed = TRUE))
    } else
    {
        # if there is no problem with the image tag then enclose
        # inside a div to preserve alignment inside table
        return(paste0("<div>", text, "</div>"))
    }
}
