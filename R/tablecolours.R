titleColour <- function()
{
    "#3E7DCC" # blue
}

subtitleColour <- function()
{
    "#A6A6A6" # gray
}

positiveSignificanceColour <- function()
{
    "#80B4F4" # light blue
}

negativeSignificanceColour <- function()
{
    "#FB9080" # light coral
}

rSquaredColour <- function()
{
    "#DDDDDD" # light gray
}

# Heatmap-like color scale
#' @importFrom formattable gradient csscolor
heatmapColourScale <- function(x, max.abs)
{
    result <- character(length(x))
    result[x > 0] <- csscolor(gradient(c(0, max.abs, pmin(x[x > 0], max.abs)), "white", positiveSignificanceColour()))[-2:-1]
    result[x < 0] <- csscolor(gradient(c(-max.abs, 0, pmax(x[x < 0], -max.abs)), negativeSignificanceColour(), "white"))[-2:-1]
    result
}
