#' Display appearance template as a \code{htmlwidget}
#'
#' @param colors A vector containing a colors as hex codes.
#' @param brand.colors A named vector containing colors with the associated brand (i.e name).
#' @param global.font A named vector specifying \code{family}, \code{color}, \code{size}, \code{units}.
#' @param fonts A list of fonts (each entry containing the font family, font color and font size).
#' @importFrom rhtmlMetro Box
#' @export
ShowTemplateOptions <- function(colors, brand.colors, global.font, fonts)
{
    html <- '
<style>
.box {
   margin: 5px;
   padding: 20px;
   display: inline-block;
}

.main-container{
    background: white;
    height: 100%;
    overflow-y: auto;
    overflow-x: hidden;
}
</style>

<div class=\"main-container\">'

    if (length(colors) > 0)
    {
        html <- paste0(html, '<h2>Colors</h2>
    <div>This palette will be used when <b>Default or template settings</b> is selected</div>')
        for (cc in colors)
            html <- paste0(html, '<span class="box" style="background-color:', cc,
                     ';float:left;">', cc, '</span>')
        html <- paste0(html, '<div style="clear: both;"></div>')
    }

    if (length(brand.colors) > 0)
    {
        html <- paste0(html, '<h2>Brand colors</h2>
    <div>These colors will be used when <b>Brand colors</b> is used and the category names in the chart match the brand names.</div>')
        for (ci in 1:length(brand.colors))
            html <- paste0(html, '<span class="box" style="background-color:', brand.colors[ci],
                     ';float:left;">', names(brand.colors)[ci], '</span>')
        html <- paste0(html, '<div style="clear: both;"></div>')
    }

    if (length(fonts) > 0)
    {
        f.sc <- if (global.font$units %in% c("pt", "points")) 1.3333 else 1
        html <- paste0(html, '<h2>Fonts</h2>
    <div>The following fonts will used in their respective text elements if <b>Default and template font settings</b> is selected.')
        for (fi in 1:length(fonts))
           html <- paste0(html, '<div style="font-family:', fonts[[fi]]$family, '; font-size:',
                          round(fonts[[fi]]$size * f.sc, 0), 'px; text-align: center; color:', fonts[[fi]]$color, '">', names(fonts)[fi], '</div>')
    }
    html <- paste0(html, '</div>\n')


    Box(html, text.as.html = TRUE,
                        font.family = "Circular, Arial, sans-serif",
                        font.size = 8)
}
