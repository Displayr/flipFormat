#' Display appearance template as a \code{htmlwidget}
#'
#' @param colors A vector containing a colors as hex codes.
#' @param brand.colors A named vector containing colors with the associated brand (i.e name).
#' @param global.font A named vector specifying \code{family}, \code{color}, \code{size}, \code{units}.
#' @param global.number.font A named vector specifying  \code{units}.
#' @param fonts A list of fonts (each entry containing the font family, font color and font size).
#' @param number.fonts A list of fonts (each entry containing the font family, font color and font size and optionally, weight and bg.color).
#' @importFrom rhtmlMetro Box
#' @export
ShowTemplateOptions <- function(colors = NULL, brand.colors = NULL,
    global.font = NULL, fonts = NULL,
    global.number.font = list(units = "pt"), number.fonts = NULL)
{
    html <- '
<style>
.displayr-color-palette-template-box{
   margin: 5px;
   padding: 20px;
   display: inline-block;
}

.displayr-color-palette-template-container{
    background: white;
    height: 100%;
    overflow-y: auto;
    overflow-x: hidden;
    line-height: normal;
}
</style>

<div class=\"displayr-color-palette-template-container\">'

    if (length(colors) > 0)
    {
        html <- paste0(html, '<h2>Colors</h2>
    <div>This palette will be used when <b>Default or template settings</b> is selected as the color palette.</div>')
        c.text <- if (is.null(names(colors))) colors else names(colors)
        for (cci in 1:length(colors))
            html <- paste0(html, '<span class="displayr-color-palette-template-box" style="background-color:', colors[cci],
                     ';float:left;">', c.text[cci], '</span>')
        html <- paste0(html, '<div style="clear: both;"></div>')
    }

    if (length(brand.colors) > 0)
    {
        html <- paste0(html, '<h2>Brand colors</h2>
    <div>These colors will be used when <b>Brand colors</b> is selected as the color palette and the category names in the chart match the brand names.</div>')
        for (ci in 1:length(brand.colors))
            html <- paste0(html, '<span class="displayr-color-palette-template-box',
                           '" style="background-color:', brand.colors[ci],
                           ';float:left;">', names(brand.colors)[ci], '</span>')
        html <- paste0(html, '<div style="clear: both;"></div>')
    }

    if (length(fonts) > 0)
    {
        f.sc <- if (global.font$units %in% c("pt", "points")) 1.3333 else 1
        html <- paste0(html, '<h2>Chart fonts</h2>
    <div>The following fonts will used in Visualization charts if <b>Default and template font settings</b> is selected.')
        for (fi in 1:length(fonts))
           html <- paste0(html, '<div style="font-family:', fonts[[fi]]$family, '; font-size:',
                          round(fonts[[fi]]$size * f.sc, 0), 'px; text-align: center; color:', fonts[[fi]]$color, '">', names(fonts)[fi], '</div>')
        html <- paste0(html, '<div style="clear: both;"></div>')
    }
    html <- paste0(html, '</div>\n')

    if (length(number.fonts) > 0)
    {
        f.sc <- if (global.number.font$units %in% c("pt", "points")) 1.3333 else 1
        html <- paste0(html, '<h2>Number fonts</h2>
    <div>The following fonts will used in <b>Visualization - Number</b> outputs if <b>Default and template font settings</b> is selected.')
        for (fi in 1:length(number.fonts))
        {
            tmp.weight <- if (is.null(number.fonts[[fi]]$weight)) "Normal" else number.fonts[[fi]]$weight
            tmp.bg <- if (is.null(number.fonts[[fi]]$bg.color)) "#FFFFFF" else number.fonts[[fi]]$bg.color
            html <- paste0(html, '<div style="text-align:center"><span style="font-family:', number.fonts[[fi]]$family, '; font-weight:',
                          tmp.weight, '; background-color:', tmp.bg, '; font-size:',
                          round(number.fonts[[fi]]$size * f.sc, 0), 'px; color:',
                          number.fonts[[fi]]$color, '">', names(number.fonts)[fi], '</span></div>')
        }
        html <- paste0(html, '<div style="clear: both;"></div>')
    }
    html <- paste0(html, '</div>\n')

    boxIframeless(html, text.as.html = TRUE,
                  font.family = "Circular, Arial, sans-serif",
                  font.size = 8)
}
