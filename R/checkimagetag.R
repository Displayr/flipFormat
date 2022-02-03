#' @importFrom httr GET content
checkImageTag <- function(text)
{
    if (grepl("src=[\"']{2}", text))
    {
        warning("Image tag contains blank source")
        return("")
    }
    return(paste0("<div>", text, "</div>"))

    #  Rest is ignored
    patt <- "<img src=([\"'])((?:\\\\?+.)*?)\\1>"
    mm <- regexpr(patt, text, perl = TRUE)
    if (mm < 0)
    {
        patt2 <- "<img src=([^>]+)>"
        mm2 <- regexpr(patt2, text, perl = TRUE)
        if (mm2 < 0)
        {
            warning("Table content contains an image tag with a syntax error which has been removed: ", text)
            return("")
        }
        imgtag <- substr(text, mm2, mm2 + attr(mm2, "match.length") - 1)
        imglink <- strsplit(substr(text, attr(mm2, "capture.start")[1], attr(mm2, "capture.start")[1] +
                                       attr(mm2, "capture.length")[1] - 1), " ")[[1]][1]

    } else
    {
        imgtag <- substr(text, mm, mm + attr(mm, "match.length") - 1)
        imglink <- substr(text, attr(mm, "capture.start")[2], attr(mm, "capture.start")[2] + attr(mm, "capture.length")[2] - 1)
    }
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
