#' Display a html table with custom formatting
#' @description Displays html table with custom formatting. This can be specified
#'  separately for each cell be specifying attributes or using CSS.
#' @param x Matrix or data frame of contents to show in the table
#' @param sig.change.fills Matrix of same dim as \code{x} used for cell fills
#'  (1 denotes increase/green fill, -1 denotes decrease/red fill, 0 no fill)
#' @param sig.change.arrows Matrix of same dim as \code{x} used for cell arrows
#'  (1 denotes increase/green up arrow, -1 denotes decrease/red down arrow, 0 no arrow)
#' @param sig.leader.circles Matrix of same dim as \code{x} used for 'leader' circles
#'  (2 denotes row leader, 1 denotes tied leaders, 0 denotes no circle)
#' @param format.type One of "Automatic", "Percentage (multiply by 100
#'  and add percentage sign) or "Numeric". When set to "Automatic", the format type
#'  will be determined by \code{attr(x, "statistic")}. Ignored if \code{x} is not numeric.
#' @param format.show.pct.sign Show percentage sign when \code{format.type} is "Percentage".
#' @param format.decimals Controls number of decimal places shown in table cells.
#'  Ignored if \code{x} is not numeric.
#' @param transpose Whether to switch rows and columns in \code{x}.
#' @param global.font.family Character; font family for all occurrences of any
#'  font attribute in the table unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#'  (e.g. "black") or an a hex code.
#' @param font.unit One of "px" of "pt". By default all font sizes are specified in terms of
#'  pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#'  points ("pt"), which will be consistent with font sizes in text boxes.
#' @param cell.prefix Character value/vector/matrix that is prepended before the cell values.
#' @param cell.suffix Character value/vector/matrix that is appended after the cell values.
#' @param cell.fill Background color of the cells in the table.
#' @param cell.border.width Width of border around table cells (in pixels).
#' @param cell.border.color Color of border around table cells,
#' @param cell.align.horizontal Horizontal alignment of text in table cells.
#' @param cell.align.vertical Vertical alignment of text in table cells.
#' @param cell.font.family Font family of text in table cells.
#' @param cell.font.color Font color of text in table cells.
#' @param cell.font.size Font size (in pixels) of text in table cells.
#' @param cell.font.weight One of "normal" or "bold".
#' @param cell.font.style One of "normal" or "italic".
#' @param cell.pad Space between text and cell border in pixels. This is only used if the
#'  horizontal alignment is "left" or "right".
#' @param show.col.headers Logical; whether to show column headers in the table.
#'  This will be ignored if \code{x} does not contain column names.
#' @param col.header.labels A vector or comma-separated labels to override the
#'  column names of \code{x}.
#' @param col.header.fill Background color of the column headers in the table.
#' @param col.header.border.width Width of border around table column headers (in pixels).
#' @param col.header.border.color Color of border around table column headers,
#' @param col.header.align.horizontal Horizontal alignment of text in table column headers.
#' @param col.header.align.vertical Vertical alignment of text in table column headers.
#' @param col.header.font.family Font family of text in table column headers.
#' @param col.header.font.color Font color of text in table column headers.
#' @param col.header.font.size Font size (in pixels) of text in table column headers.
#' @param col.header.font.weight One of "normal" or "bold".
#' @param col.header.font.style One of "normal" or "italic".
#' @param col.header.pad Space between text and cell border in pixels. This is only used if the
#'  horizontal alignment is "left" or "right".
#' @param show.row.headers Logical; whether to show row headers in the table.
#'  This will be ignored if \code{x} does not contain row names.
#' @param row.header.labels A vector or comma-separated labels to override
#'   the column names of \code{x}.
#' @param row.header.fill Background color of the row headers in the table.
#' @param row.header.border.width Width of border around table row headers (in pixels).
#' @param row.header.border.color Color of border around table row headers,
#' @param row.header.align.horizontal Horizontal alignment of text in table row headers.
#' @param row.header.align.vertical Vertical alignment of text in table row headers.
#' @param row.header.font.family Font family of text in table row headers.
#' @param row.header.font.color Font color of text in table row headers.
#' @param row.header.font.size Font size (in pixels) of text in table row headers.
#' @param row.header.font.weight One of "normal" or "bold".
#' @param row.header.font.style One of "normal" or "italic".
#' @param row.header.pad Space between text and cell border in pixels. This is only used if the
#'  horizontal alignment is "left" or "right".
#' @param row.span.fill Background color of the row.spans in the table.
#' @param row.span.border.width Width of border around table row.spans (in pixels).
#' @param row.span.border.color Color of border around table row.spans,
#' @param row.span.align.horizontal Horizontal alignment of text in table row.spans.
#' @param row.span.align.vertical Vertical alignment of text in table row.spans.
#' @param row.span.font.family Font family of text in table row.spans.
#' @param row.span.font.color Font color of text in table row.spans.
#' @param row.span.font.size Font size (in pixels) of text in table row.spans.
#' @param row.span.font.weight One of "normal" or "bold".
#' @param row.span.font.style One of "normal" or "italic".
#' @param row.span.pad Space between text and cell border in pixels. This is only used if the
#'  horizontal alignment is "left" or "right".
#' @param col.header.classes CSS classes of column headers. The class definition should be added to
#'  \code{custom.css}. This overrides \code{col.header.fill},
#'  \code{col.header.border}, \code{col.header.font}, \code{col.header.align}, etc.
#' @param row.header.classes CSS classes of column headers. This overrides \code{row.header.fill},
#'  \code{row.header.border}, \code{row.header.font}, \code{row.header.align}, etc
#' @param col.classes any specific column classes to apply. e.g. \code{list(list(ix=3, class="bluefill"))}
#'  will cause column 3 to have class "bluefill".
#' @param row.classes any specific row classes to apply.
#' @param col.widths specify column widths in \% or px; Remaining width divided between remaining columns.
#' @param corner Contents of the corner cell, if row and column headers are used
#' @param corner.class Class of the corner cell, if row and column headers are used
#' @param corner.fill Background color of the corners in the table.
#' @param corner.border.width Width of border around table corners (in pixels).
#' @param corner.border.color Color of border around table corners,
#' @param corner.align.horizontal Horizontal alignment of text in table corners.
#' @param corner.align.vertical Vertical alignment of text in table corners.
#' @param corner.font.family Font family of text in table corners.
#' @param corner.font.color Font color of text in table corners.
#' @param corner.font.size Font size (in pixels) of text in table corners.
#' @param corner.font.weight One of "normal" or "bold".
#' @param corner.font.style One of "normal" or "italic".
#' @param corner.pad Space between text and cell border in pixels. This is only used if the
#'  horizontal alignment is "left" or "right".
#' @param banded.rows Whether to have banded rows
#' @param banded.cols Whether to have banded columns
#' @param banded.odd.fill Background of cells in odd rows or columns when \code{banded.rows} or \code{banded.cols}.
#' @param banded.even.fill Background of cells in even rows or columns when \code{banded.rows} or \code{banded.cols}.
#' @param sig.fills.up Cell color when \code{sig.change.fills} is used.
#' @param sig.fills.down Cell color when \code{sig.change.fills} is used.
#' @param sig.arrows.up Color of up arrows when \code{sig.change.arrows} is used.
#' @param sig.arrows.down Color of down arrows when \code{sig.change.arrows} is used.
#' @param circle.size Size of circles when \code{sig.leader.circles} is used.
#' @param spacer.row Indices of any blank divider rows
#' @param spacer.col Indices of any blank divider columns
#' @param row.height Height of table body rows. If \code{NULL}, then the rows are stretched to fill container.
#' @param col.header.height Height of table header rows
#' @param col.spans List of column spans to place above the column headers:
#'  list(list(width=,label=,class=), list(width=,label=,class=))
#' @param row.spans List of row spans to place left of the row headers: list(list(height=,label=,class=),
#'  list(height=,label=,class=)
#' @param custom.css Any custom CSS to add to the \code{<style>} header of the html
#'  (e.g. defining nth-child logic or custom classes not included in the CSS function)
#' @param use.predefined.css Logical; whether to include CSS definitions for classes
#'  \code{rh, rhclean, simpleheader, simpleheaderclean, nsline, subjourneyHeader, subjourneySubHeader
#'  white, spacer}. This is included for backwards compatibiliy but it is probably safer
#'  to omit this is not used.
#' @param suppress.nan whether to empty cells containing only NaN
#' @param suppress.na whether to empty cells containing only NA
#' @param resizable Allow column widths to be resizeable by dragging with mouse.
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @examples
#' xx <- structure(1:24, .Dim = c(4L, 6L), .Dimnames = list(c("a", "b", "c", "d"),
#'          c("A", "B", "C", "D", "E", "F")))
#' CreateCustomTable(xx, row.spans=list(list(height=2, label="AA"),
#'          list(height=1, label="BB"), list(height=1, label="CC")))
#' @export
CreateCustomTable = function(x,
                        sig.change.fills = NULL,
                        sig.change.arrows = NULL,
                        sig.leader.circles = NULL,
                        format.type = "Automatic",
                        format.show.pct.sign = TRUE,
                        format.decimals = 0,
                        suppress.nan = TRUE,
                        suppress.na = TRUE,
                        transpose = FALSE,
                        col.widths = c("25%"),
                        row.height = NULL,
                        col.header.height = "35px",
                        global.font.family = "Arial",
                        global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        font.unit = "px",
                        cell.prefix = "",
                        cell.suffix = "",
                        cell.fill = "#FFFFFF",
                        cell.border.width = 1,
                        cell.border.color = "#FFFFFF",
                        cell.align.horizontal = "center",
                        cell.align.vertical = "middle",
                        cell.font.family = global.font.family,
                        cell.font.color = global.font.color,
                        cell.font.size = 14,
                        cell.font.weight = "normal",
                        cell.font.style = "normal",
                        cell.pad = 0,
                        show.col.headers = TRUE,
                        col.header.labels = NULL,
                        col.header.fill = "#DCDCDC",
                        col.header.border.width = 1,
                        col.header.border.color = "#FFFFFF",
                        col.header.align.horizontal = "center",
                        col.header.align.vertical = "middle",
                        col.header.font.family = global.font.family,
                        col.header.font.color = global.font.color,
                        col.header.font.size = 14,
                        col.header.font.weight = "bold",
                        col.header.font.style = "normal",
                        col.header.pad = 0,
                        show.row.headers = TRUE,
                        row.header.labels = NULL,
                        row.header.fill = "#FFFFFF",
                        row.header.border.width = 1,
                        row.header.border.color = col.header.border.color,
                        row.header.align.horizontal = "left",
                        row.header.align.vertical = "middle",
                        row.header.font.family = global.font.family,
                        row.header.font.color = global.font.color,
                        row.header.font.size = 14,
                        row.header.font.style = "normal",
                        row.header.font.weight = "bold",
                        row.header.pad = 0,
                        row.span.fill = "#FFFFFF",
                        row.span.border.width = 1,
                        row.span.border.color = col.header.border.color,
                        row.span.align.horizontal = "left",
                        row.span.align.vertical = "middle",
                        row.span.font.family = global.font.family,
                        row.span.font.color = global.font.color,
                        row.span.font.size = 14,
                        row.span.font.style = "normal",
                        row.span.font.weight = "bold",
                        row.span.pad = 0,
                        corner = "",
                        corner.class = "",
                        corner.fill = "#FFFFFF",
                        corner.border.width = col.header.border.width,
                        corner.border.color = col.header.border.color,
                        corner.align.horizontal = "center",
                        corner.align.vertical = "middle",
                        corner.font.family = global.font.family,
                        corner.font.color = global.font.color,
                        corner.font.size = 14,
                        corner.font.weight = "bold",
                        corner.font.style = "normal",
                        corner.pad = 0,
                        col.header.classes = "",
                        row.header.classes = "",
                        col.classes = list(),
                        row.classes = list(),
                        banded.rows = FALSE,
                        banded.cols = FALSE,
                        banded.odd.fill = 'rgb(250,250,250)',
                        banded.even.fill = 'rgb(245,255,245)',
                        sig.fills.up = 'rgb(195,255,199)',
                        sig.fills.down = 'rgb(255,213,213)',
                        sig.arrows.up = 'rgb(0,172,62)',
                        sig.arrows.down = 'rgb(192,0,0)',
                        circle.size = 35,
                        spacer.row = NULL,
                        spacer.col = NULL,
                        col.spans = NULL,
                        row.spans = NULL,
                        custom.css = '',
                        use.predefined.css = TRUE,
                        resizable = FALSE)
{
    # Check input
    x <- tidyMatrixValues(x, transpose, row.header.labels, col.header.labels)
    nrows <- nrow(x)
    ncols <- ncol(x)
    if (is.null(colnames(x)))
        show.col.headers <- FALSE
    if (is.null(rownames(x)))
        show.row.headers <- FALSE

    # Format table contents
    if (format.type == "Automatic" && any(grepl("%", attr(x, "statistic"))))
        format.type <- "Percentage"

    content <- if (!is.numeric(x))                   x
               else if (format.type == "Percentage") FormatAsPercent(x, decimals = format.decimals, show.sign = format.show.pct.sign)
               else                                  FormatAsReal(x, decimals = format.decimals)
    content <- matrix(paste0(cell.prefix, content, cell.suffix), nrows, ncols)
    if (suppress.nan)
        content[which(is.nan(x))] <- ""
    if (suppress.na)
        content[which(is.na(x) & !is.nan(x))] <- ""

    # Significance testing arrows/circles/fills
    if (!is.null(sig.change.arrows))
    {
        content[which(sig.change.arrows ==  1)] <- paste0(content[which(sig.change.arrows ==  1)],
                    "<font style='color:", sig.arrows.up, "'>&#x2191;</font>")
        content[which(sig.change.arrows == -1)] <- paste0(content[which(sig.change.arrows == -1)],
                    "<font style='color:", sig.arrows.down, "'>&#x2193;</font>")
    }
    circle.css <- ""
    if (!is.null(sig.leader.circles))
    {
        metric.leader.border = '2px solid rgb(120,120,120)'
        metric.tie.border = '1px solid rgb(150,150,150)'
        circle.fmt <- paste0('display: inline-block; line-height:', circle.size, 'px; border-radius:',
                            circle.size, 'px; height: ', circle.size, 'px; width:', circle.size, 'px;')
        circle.css <- paste0('.circle2 {  border: ', metric.leader.border, ';', circle.fmt, '}\n',
                             '.circle1 {  border: ', metric.tie.border, ';', circle.fmt, '}\n',
                             '.circle0 {  border: 0px solid rgb(0,0,0;', circle.fmt, '}\n')

        sig.leader.circles[!which(sig.leader.circles == 1 | sig.leader.circles == 2)] <- 0
        content <- matrix(sprintf('<div class="circle%s">%s</div>', sig.leader.circles, content), nrows, ncols)
    }
    if (!banded.rows && !banded.cols)
        cell.fill <- matrix(paste("background:", cell.fill, ";"), nrows, ncols)
    else
        cell.fill <- matrix("", nrows, ncols)
    if (!is.null(sig.change.fills))
    {
        cell.fill[which(sig.change.fills ==  1)] <- paste("background:", sig.fills.up, ";")
        cell.fill[which(sig.change.fills == -1)] <- paste("background:", sig.fills.down, ";")
    }

    # Set up styles for each cell - vector/matrix values automatically recycled
    cell.styles <- paste0("style = '", cell.fill,
        "border: ", cell.border.width, "px solid ", cell.border.color,
        ";", getPaddingCSS(tolower(cell.align.horizontal), cell.pad),
        "; font-size: ", cell.font.size, font.unit, "; font-style: ", cell.font.style,
        "; font-weight: ", cell.font.weight, "; font-family: ", cell.font.family,
        "; color:", cell.font.color, "; text-align: ", cell.align.horizontal,
        "; vertical-align: ", cell.align.vertical, ";'")
    cell.styles <- matrix(cell.styles, nrow = nrows)

    # Row/column classes overrides other attributes (except coloring based on significance)
    for (cc in row.classes)
    {
        if (!is.null(sig.change.fills))
        {
            ind <- which(abs(sig.change.fills[cc[[1]],]) < 1)
            cell.styles[cc[[1]], ind] <- sprintf('class="%s"', cc[[2]])
        }
        else
            cell.styles[cc[[1]],] = sprintf('class="%s"',cc[[2]])
    }
    for (cc in col.classes)
    {
        if (!is.null(sig.change.fills))
        {
            ind <- which(abs(sig.change.fills[,cc[[1]]]) < 1)
            cell.styles[cc[[1]], ind] <- sprintf('class="%s"', cc[[2]])
        }
        else
            cell.styles[,cc[[1]]] = sprintf('class="%s"',cc[[2]])
    }

    # Row headers
    if (show.row.headers)
    {
        if (sum(nchar(row.header.classes)) == 0)
            row.header.styles <- paste0("style = 'background: ", row.header.fill,
                "; border: ", row.header.border.width, "px solid ", row.header.border.color,
                ";", getPaddingCSS(tolower(row.header.align.horizontal), row.header.pad),
                "; font-size: ", row.header.font.size, font.unit, "; font-style: ", row.header.font.style,
                "; font-weight: ", row.header.font.weight, "; font-family: ", row.header.font.family,
                "; color:", row.header.font.color, "; text-align: ", row.header.align.horizontal,
                "; vertical-align: ", row.header.align.vertical, ";'")
        else
            row.header.styles <- sprintf('class="%s"', row.header.classes)
        content <- cbind(rownames(x), content)
        cell.styles <- cbind(row.header.styles, cell.styles)
    } else { corner = NULL; corner.class = NULL; }

    # Row spans
    if (!is.null(row.spans))
    {
        span.lengths <- sapply(row.spans, function(x) x[['height']])
        row.span.styles <- paste0("style = 'background: ", row.span.fill,
            "; border: ", row.span.border.width, "px solid ", row.span.border.color,
            ";", getPaddingCSS(tolower(row.span.align.horizontal), row.span.pad),
            "; font-size: ", row.span.font.size, font.unit, "; font-style: ", row.span.font.style,
            "; font-weight: ", row.span.font.weight, "; font-family: ", row.span.font.family,
            "; color:", row.span.font.color, "; text-align: ", row.span.align.horizontal,
            "; vertical-align: ", row.span.align.vertical, ";'")
        row.span.styles <- rep(row.span.styles, length = length(row.spans))
        for (i in 1:length(row.spans))
            if (!is.null(row.spans[[i]]$class))
                row.span.styles[i] <- sprintf('class="%s"', row.spans[[i]]$class)

        row.spans <- sapply(1:length(row.spans), function(i) sprintf('<td rowspan="%s" %s>%s</td>',
                            row.spans[[i]][['height']], row.span.styles[i], row.spans[[i]][['label']]))
        row.span.html <- rep("", nrows)
        j <- 1
        for (i in 1:length(row.spans))
        {
            row.span.html[j] <- row.spans[i]
            j <- j + span.lengths[i]
        }

    } else
        row.span.html <- ''

    # Column headers
    if (show.col.headers)
    {
        if (sum(nchar(col.header.classes)) == 0)
            col.header.styles <- paste0("style = 'background: ", col.header.fill,
                "; border: ", col.header.border.width, "px solid ", col.header.border.color,
                ";", getPaddingCSS(tolower(col.header.align.horizontal), col.header.pad),
                "; font-size: ", col.header.font.size, font.unit, "; font-style: ", col.header.font.style,
                "; font-weight: ", col.header.font.weight, "; font-family: ", col.header.font.family,
                "; color:", col.header.font.color, "; text-align: ", col.header.align.horizontal,
                "; vertical-align: ", col.header.align.vertical, ";'")
        else
             col.header.styles <- sprintf('class="%s"', col.header.classes)
        col.header.styles <- rep(col.header.styles, length = ncols)
        col.labels <- colnames(x)

        if (show.row.headers)
        {
            if (sum(nchar(corner.class)) == 0)
            corner.styles <- paste0("style = 'background: ", corner.fill,
                "; border: ", corner.border.width, "px solid ", corner.border.color,
                ";", getPaddingCSS(tolower(corner.align.horizontal), corner.pad),
                "; font-size: ", corner.font.size, font.unit, "; font-style: ", corner.font.style,
                "; font-weight: ", corner.font.weight, "; font-family: ", corner.font.family,
                "; color:", corner.font.color, "; text-align: ", corner.align.horizontal,
                "; vertical-align: ", corner.align.vertical, ";'")
            else
                corner.styles <- sprintf('class="%s"', corner.class)
            col.header.styles <- c(corner.styles[1], col.header.styles)
            col.labels <- c(corner, col.labels)
        }

        if (!is.null(row.spans))
        {
            col.header.styles <- c("", col.header.styles)
            col.labels <- c("", col.labels)
        }
        if (!is.null(spacer.col))
            col.class.html[spacer.col] <- 'class = "spacer"'
        if (!is.null(spacer.row))
            spacer.row <-  spacer.row + 1
        header.html <- paste0(c('<tr>', sprintf('<th %s>%s</th>', col.header.styles, col.labels),
                                '</tr>'), collapse='')
    } else
        header.html <- ''

    # Column spans
    if (!is.null(col.spans))
    {
        spans <- append(list(list(width = 1, label = '', class = 'spacer')), col.spans)
        spans <- sapply(spans, function(cc) sprintf('<th colspan="%s" class="%s">%s</th>',
                        cc[['width']], cc[['class']], cc[['label']]))
        col.span.html <- paste0('<tr>', paste0(spans, collapse=''),'</tr>')
    } else
        col.span.html <- ''

    # Setup html file
    tfile <- createTempFile()
    cata <- createCata(tfile)
    cata("<style>\n")
    cata("table { border-collapse: collapse; table-layout: fixed; ",
                 "font-family: ", global.font.family, "; color: ", global.font.color, "; ",
                 "white=space:nowrap; cellspacing:'0'; cellpadding:'0'; }\n")
    cata("thead, th { overflow: hidden; ")
    if (resizable)
        cata("resize: both; ")
    if (sum(nchar(col.header.height)) > 0)
        cata("height:", col.header.height, "; ")
    cata("}\n")
    cata("td { overflow: hidden; ")
    if (sum(nchar(row.height)) > 0)
        cata("height:", row.height, "; ")
    cata("}\n")

    # Row/Column banding
    if (banded.rows)
        cata('tbody tr:nth-child(odd){background-color:', banded.odd.fill,
                ';} tr:nth-child(even){background-color:', banded.even.fill, ';}')
    if (banded.cols)
        cata('tbody td:nth-child(2n+3){background-color:', banded.odd.fill,
             ';} td:nth-child(even){background-color:', banded.even.fill, ';}')

    # Other CSS
    if (use.predefined.css)
        cata("\n", predefinedCSS(), "\n")
    cata("\n", circle.css, "\n")
    cata("\n", custom.css, "\n")

    cata("</style>\n\n")
    table.height <- if (sum(nchar(row.height)) != 0) ""
                    else paste0("; height:calc(100% - ", rev(cell.border.width)[1], "px)")
    cata(sprintf("<table style = 'width:calc(%s - %dpx)%s'>\n", "100%",
         max(0, max(cell.border.width)), table.height))
    if (sum(nchar(col.widths)) > 0)
    {
        col.widths <- ConvertCommaSeparatedStringToVector(col.widths)
        cata(paste(paste("<col width='", col.widths, "'>\n"), collapse = ""))
    }
    cata('<thead>', col.span.html, header.html, '</thead>')


    # Build table
    cell.html <- matrix(sprintf('<td %s>%s</td>', cell.styles, content), nrow = nrows)
    cell.html <- cbind(row.span.html, cell.html)
    body.html <- paste0(sprintf('<tr>%s</tr>',
                    apply(cell.html, 1, paste0, collapse = '')), collapse='')

    cata(body.html)
    cata("</table>\n")
    html <- paste(readLines(tfile), collapse = "\n")
    out <- rhtmlMetro::Box(html, text.as.html = TRUE,
                    font.family = "Circular, Arial, sans-serif",
                    font.size = 8)

    attr(out, "ChartData") <- x
    return(out)
}


tidyMatrixValues <- function(x, transpose, row.header.labels, col.header.labels)
{
    x <- as.matrix(x)
    if (transpose)
        x <- t(x)

    if (length(row.header.labels) < nrow(x))
        row.header.labels <- ConvertCommaSeparatedStringToVector(row.header.labels)
    if (length(row.header.labels) > 0)
    {
        new.labels <- paste0(rownames(x), rep("", nrow(x))) # in case rownames is NULL
        tmp.len <- min(length(row.header.labels), length(new.labels))
        new.labels[1:tmp.len] <- row.header.labels[1:tmp.len]
        rownames(x) <- new.labels
    }
    if (length(col.header.labels) > ncol(x))
        col.header.labels <- ConvertCommaSeparatedStringToVector(col.header.labels)
    if (length(col.header.labels) > 0)
    {
        new.labels <- paste0(colnames(x), rep("", ncol(x)))
        tmp.len <- min(length(col.header.labels), length(new.labels))
        new.labels[1:tmp.len] <- col.header.labels[1:tmp.len]
        colnames(x) <- new.labels
    }
    return(x)
}

getPaddingCSS <- function(align, pad)
{
    if (length(align) < length(pad))
        align <- rep(align, length = length(pad))
    ind <- which(align %in% c("left", "right"))

    # Center alignment does not use padding
    if (length(ind) == 0)
        return("")

    res <- rep("", length = length(align))
    res[ind] <- paste0("padding-", align[ind], ":", pad, "px")
    return(res)
}

predefinedCSS <- function()
{
"
    .rh {
        text-align:left;
        font-weight: bold;
        }
    .rhclean {
        text-align:left;
        }
    .simpleheader {
        background-color: #DCDCDC;
        font-weight: bold;
        }
    .simpleheaderclean {
        background-color: #DCDCDC;
        font-weight: normal;
        }
    .nsline {
        font-style: italic;
        font-size: 9pt;
        white-space:nowrap;
        display: block;}
    .subjourneyHeader{
        font-style: bold;
        border-bottom: 1px black solid;
        }
    .subjourneySubHeader{
        font-style: bold;
        border-top: 1px grey solid;
        border-bottom: 1px grey solid;
        }
    .white {background-color:white;}
    .spacer {background: white;color: white;border: none;overflow:hidden;}
"
}
