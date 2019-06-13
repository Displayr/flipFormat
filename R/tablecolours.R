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

positiveSignificanceLighter <- function()
{
    "#E8F1FD" # lighter blue
}

negativeSignificanceColour <- function()
{
    "#FB9080" # light coral
}

barColour <- function()
{
    "#DDDDDD" # light gray
}

positiveColour <- function()
{
    "#4894F2" # blue
}

negativeColour <- function()
{
    "#F95F4A" # coral
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

# Heatmap-like color scale (single colour)
#' @importFrom formattable gradient csscolor
heatmapSingleColourScale <- function(x, min.val, max.val)
{
    x[is.na(x)] <- min.val
    csscolor(gradient(c(min.val, max.val, x), positiveSignificanceLighter(),
                      positiveSignificanceColour()))[-2:-1]
}

#' \code{ZScoresAsColors}
#'
#' Shades z-scores.
#' @param z A \code{\link{numeric}} vector containing colors.
#' @param light.negative.color The color, in standard sRGB colorspace, representing the least negative value.
#' @param dark.negative.color The color, in standard sRGB colorspace, representing the most negative value.
#' @param light.positive.color The color, in standard sRGB colorspace, representing the least positive value.
#' @param dark.positive.color The color, in standard sRGB colorspace, representing the most positive value.
#' @param non.significant.color The color used for non-significant values.
#' @param max.abs The (absolute) value beyond which all colors are set to the \code{negative.color} or \code{positive.color}
#' @param alpha The level of significance, where values not significant at this value are shaded with the \code{non.significant.color}.
#' @importFrom grDevices colorRampPalette colorRamp rgb
#' @export
#'
ZScoresAsColors <- function(z,
                            light.negative.color = "#FB9080",
                            dark.negative.color = "#FA614B",
                            light.positive.color = "#80B4F4",
                            dark.positive.color = "#3E7DCC",
                            non.significant.color = "#888888",
                            max.abs = 5,
                            alpha = 0.05)
{
    negative.colors <- colorRamp(c((light.negative.color), (dark.negative.color)))
    positive.colors <- colorRamp(c((light.positive.color), (dark.positive.color)))
    # Capping z's bigger than max.abs to max.abs
    z[z > max.abs] = max.abs
    z[z < -max.abs] = -max.abs
    # Settp,g z's not significant to 0
    z[z > qnorm(alpha / 2) & z < -qnorm(alpha / 2)] = 0
    # Rescaling
    z.rescaled <- z / max.abs
    result <- positive.colors(abs(z.rescaled))
    result[z < 0] <- negative.colors(abs(z.rescaled[z < 0]))
    result <- rgb(result / 255)
    result[z == 0] <- non.significant.color
    names(result) <- names(z)
    result
}

officialColors <- function()
{
    c("#3e7dcc", "#04b5ac", "#f5c524", "#c44e41",
      "#8cc0ff", "#ff905a", "#345e8c",
      "#04827b", "#967f47","#96362f")
}
