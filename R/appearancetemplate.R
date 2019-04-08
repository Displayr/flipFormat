#' Display appearance template as a \code{htmlwidget}
#'
#' @param x An object of class \code{AppearanceTemplate}.
#' @importFrom rhtmlMetro Box
print.AppearanceTemplate <- function(x)
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

if (!is.null(x$colors))
{
    html <- paste0(html, '<h2>Colors</h2>
<div>This palette will be used when <b>Template or default settings</b> is selected</div>')
    for (cc in x$colors)
        html <- paste0(html, '<span class="box" style="background-color:', cc,
                 ';float:left;">', cc, '</span>')
    html <- paste0(html, '<div style="clear: both;"></div>')
}

if (!is.null(x$brand.colors))
{
    html <- paste0(html, '<h2>Brand colors</h2>
<div>These colors will be used when <b>Brand colors</b> is used and the category names in the chart match the brand names.</div>')
    for (ci in 1:length(x$brand.colors))
        html <- paste0(html, '<span class="box" style="background-color:', x$brand.colors[ci],
                 ';float:left;">', names(x$brand.colors)[ci], '</span>')
    html <- paste0(html, '<div style="clear: both;"></div>')
}

if (!is.null(x$fonts))
{
    f.sc <- if (x$global.font$units %in% c("pt", "points")) 1.3333 else 1
    html <- paste0(html, '<h2>Fonts</h2>
<div>The following fonts will used in their respective text elements if <b>Template or default font settings</b> is selected.')
    for (fi in 1:length(x$fonts))
       html <- paste0(html, '<div style="font-family:', x$fonts[[fi]]$family, '; font-size:',
                      round(x$fonts[[fi]]$size * f.sc, 0), 'px; text-align: center; color:', x$fonts[[fi]]$color, '">', names(x$fonts)[fi], '</div>')
}
html <- paste0(html, '</div>\n')


Box(html, text.as.html = TRUE,
                    font.family = "Circular, Arial, sans-serif",
                    font.size = 8)
}
