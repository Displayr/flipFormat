titleColour <- function()
{
    "#3E7DCC" # blue
}

subtitleColour <- function()
{
    "#888888" # gray
}

positiveSignificanceColour <- function()
{
    "#80B4F4" # light blue
}

negativeSignificanceColour <- function()
{
    "#FB9080" # light coral
}

barColour <- function()
{
    "#DDDDDD" # light gray
}

# Heatmap-like color scale
#' @importFrom formattable gradient csscolor
heatmapColourScale <- function(x, max.abs)
{
    x[is.na(x)] <- 0
    result <- character(length(x))
    result[x > 0] <- csscolor(gradient(c(0, max.abs, pmin(x[x > 0], max.abs)), "white", positiveSignificanceColour()))[-2:-1]
    result[x < 0] <- csscolor(gradient(c(-max.abs, 0, pmax(x[x < 0], -max.abs)), negativeSignificanceColour(), "white"))[-2:-1]
    result
}
