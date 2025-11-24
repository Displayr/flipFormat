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
#' @param font.size Global font size of all elements in the table. This is provided for
#'  convenience but its overriden by the font sizes of specific components.
#' @param font.unit One of "px" of "pt". By default all font sizes are specified in terms of
#'  pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#'  points ("pt"), which will be consistent with font sizes in text boxes.
#' @param border.color Color of all borders. Will be overriden if specific elements are set.
#' @param border.width Width of borders (in pixels) in all cells. Will be overriden if specific elements are set.
#' @param border.collapse Logical; whether the borders of adjacent cells
#'   should be shown as a single line or separate lines.
#' @param border.row.gap Numeric; the space between the borders
#'   separating different rows. Only used if \code{border.collapse} is false.
#' @param border.column.gap Numeric; the space between the borders
#'   separating different columns. Only used if \code{border.collapse} is false.
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
#' @param col.span.fill Background color of the col.spans in the table.
#' @param col.span.border.width Width of border around table col.spans (in pixels).
#' @param col.span.border.color Color of border around table col.spans,
#' @param col.span.align.horizontal Horizontal alignment of text in table col.spans.
#' @param col.span.align.vertical Vertical alignment of text in table col.spans.
#' @param col.span.font.family Font family of text in table col.spans.
#' @param col.span.font.color Font color of text in table col.spans.
#' @param col.span.font.size Font size (in pixels) of text in table col.spans.
#' @param col.span.font.weight One of "normal" or "bold".
#' @param col.span.font.style One of "normal" or "italic".
#' @param col.span.pad Space between text and cell border in pixels. This is only used if the
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
#' @param footer Optional text shown as a footer below the table
#' @param footer.fill Background color of the footer in the table.
#' @param footer.height Height of the footer (ignored if no text in footer).
#' @param footer.lineheight Controls spacing between the lines of text in the
#'   footer. It can be specified in multiple ways but as a unitless number
#'   it is applied as a multiple to the font size.
#' @param footer.align.horizontal Horizontal alignment of text in table footer.
#' @param footer.align.vertical Vertical alignment of text in table footer.
#' @param footer.font.family Font family of text in table footer.
#' @param footer.font.color Font color of text in table footer.
#' @param footer.font.size Font size of text in table footer.
#' @param footer.font.weight One of "normal" or "bold".
#' @param footer.font.style One of "normal" or "italic".
#' @param banded.rows Whether to have banded rows
#' @param banded.cols Whether to have banded columns
#' @param banded.odd.fill Background of cells in odd rows or columns when \code{banded.rows} or \code{banded.cols}.
#' @param banded.even.fill Background of cells in even rows or columns when \code{banded.rows} or \code{banded.cols}.
#' @param sig.fills.up Cell color when \code{sig.change.fills} is used.
#' @param sig.fills.down Cell color when \code{sig.change.fills} is used.
#' @param sig.fills.nothing Cell color when \code{sig.change.nothing} is used.
#' @param sig.arrows.up Color of up arrows when \code{sig.change.arrows} is used.
#' @param sig.arrows.down Color of down arrows when \code{sig.change.arrows} is used.
#' @param circle.size Size of circles when \code{sig.leader.circles} is used.
#' @param spacer.row Indices of any blank divider rows
#' @param spacer.col Indices of any blank divider columns
#' @param row.height Height of table body rows. If \code{NULL}, then the rows are stretched to fill container.
#' @param num.header.rows This is the number of rows from \code{x} which always be shown at the
#'   top of the window (only used when \code{row.height} is specified.
#' @param col.header.height Height of table header rows
#' @param col.spans List of column spans to place above the column headers:
#'  list(list(width=,label=,class=), list(width=,label=,class=))
#' @param row.spans List of row spans to place left of the row headers: list(list(height=,label=,class=),
#'  list(height=,label=,class=)
#' @param custom.css Any custom CSS to add to the \code{<style>} header of the html
#'  (e.g. defining nth-child logic or custom classes not included in the CSS function).
#'  When this is used, the resulting widget is inclosed inside an iframe to avoid
#'  affecting other widgets.
#' @param use.predefined.css Logical; whether to include CSS definitions for classes
#'  \code{rh, rhclean, simpleheader, simpleheaderclean, nsline, subjourneyHeader, subjourneySubHeader
#'  white, spacer}. This is included for backwards compatibiliy but it is probably safer
#'  to omit this is not used.
#' @param suppress.nan whether to empty cells containing only NaN
#' @param suppress.na whether to empty cells containing only NA
#' @param overflow Determines behaviour of text that is too long to fit in the table cells. By default,
#'  it is set to "hidden" but change to "visible" to show overflow text.
#' @param enable.x.scroll If true, horizontal scrollbars are shown if the table is too wide.
#'  Sometimes, these scrollbars can appear even when it doesn't seem necessary.
#' @param enable.y.scroll If true, vertical scrollbar are shown. Mostly these would only be wanted
#'  if the number of rows is large and the height of the rows are fixed. But sometimes when there is
#'  a lot of text, the vertical scrollbars are also useful.
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
                        col.widths = if (is.null(rownames(x))) NULL else c("25%"),
                        row.height = NULL,
                        col.header.height = "35px",
                        num.header.rows = 0,
                        global.font.family = "Arial",
                        global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        font.size = 13,
                        font.unit = "px",
                        border.color = "#FFFFFF",
                        border.width = 1,
                        border.collapse = TRUE,
                        border.row.gap = 2,
                        border.column.gap = 2,
                        cell.prefix = "",
                        cell.suffix = "",
                        cell.fill = "#FFFFFF",
                        cell.border.width = border.width,
                        cell.border.color = border.color,
                        cell.align.horizontal = "center",
                        cell.align.vertical = "middle",
                        cell.font.family = global.font.family,
                        cell.font.color = global.font.color,
                        cell.font.size = font.size,
                        cell.font.weight = "normal",
                        cell.font.style = "normal",
                        cell.pad = 0,
                        show.col.headers = TRUE,
                        col.header.labels = NULL,
                        col.header.fill = "transparent",
                        col.header.border.width = border.width,
                        col.header.border.color = border.color,
                        col.header.align.horizontal = "center",
                        col.header.align.vertical = "middle",
                        col.header.font.family = global.font.family,
                        col.header.font.color = global.font.color,
                        col.header.font.size = font.size,
                        col.header.font.weight = "bold",
                        col.header.font.style = "normal",
                        col.header.pad = 0,
                        show.row.headers = TRUE,
                        row.header.labels = NULL,
                        row.header.fill = "transparent",
                        row.header.border.width = border.width,
                        row.header.border.color = col.header.border.color,
                        row.header.align.horizontal = "left",
                        row.header.align.vertical = "middle",
                        row.header.font.family = global.font.family,
                        row.header.font.color = global.font.color,
                        row.header.font.size = font.size,
                        row.header.font.style = "normal",
                        row.header.font.weight = "bold",
                        row.header.pad = 0,
                        row.span.fill = "transparent",
                        row.span.border.width = row.header.border.width,
                        row.span.border.color = row.header.border.color,
                        row.span.align.horizontal = "left",
                        row.span.align.vertical = "middle",
                        row.span.font.family = global.font.family,
                        row.span.font.color = global.font.color,
                        row.span.font.size = font.size,
                        row.span.font.style = "normal",
                        row.span.font.weight = "bold",
                        row.span.pad = 0,
                        col.span.fill = "transparent",
                        col.span.border.width = col.header.border.width,
                        col.span.border.color = col.header.border.color,
                        col.span.align.horizontal = "center",
                        col.span.align.vertical = "middle",
                        col.span.font.family = global.font.family,
                        col.span.font.color = global.font.color,
                        col.span.font.size = font.size,
                        col.span.font.style = "normal",
                        col.span.font.weight = "bold",
                        col.span.pad = 0,
                        corner = "",
                        corner.class = "",
                        corner.fill = "transparent",
                        corner.border.width = col.header.border.width,
                        corner.border.color = col.header.border.color,
                        corner.align.horizontal = "center",
                        corner.align.vertical = "middle",
                        corner.font.family = global.font.family,
                        corner.font.color = global.font.color,
                        corner.font.size = font.size,
                        corner.font.weight = "bold",
                        corner.font.style = "normal",
                        corner.pad = 0,
                        footer = "",
                        footer.height = paste0(footer.font.size + 5, font.unit),
                        footer.lineheight = "normal",
                        footer.fill = "transparent",
                        footer.align.horizontal = "center",
                        footer.align.vertical = "bottom",
                        footer.font.family = global.font.family,
                        footer.font.color = global.font.color,
                        footer.font.size = 8,
                        footer.font.weight = "normal",
                        footer.font.style = "normal",
                        col.header.classes = "",
                        row.header.classes = NULL,
                        col.classes = list(),
                        row.classes = list(),
                        banded.rows = FALSE,
                        banded.cols = FALSE,
                        banded.odd.fill = 'rgb(250,250,250)',
                        banded.even.fill = 'rgb(245,245,245)',
                        sig.fills.up = 'rgb(195,255,199)',
                        sig.fills.down = 'rgb(255,213,213)',
                        sig.fills.nothing = 'rgb(255,255,255)',
                        sig.arrows.up = 'rgb(0,172,62)',
                        sig.arrows.down = 'rgb(192,0,0)',
                        circle.size = 35,
                        spacer.row = NULL,
                        spacer.col = NULL,
                        col.spans = NULL,
                        row.spans = NULL,
                        overflow = "hidden",
                        enable.x.scroll = FALSE,
                        enable.y.scroll = !is.null(row.height),
                        custom.css = '',
                        use.predefined.css = TRUE,
                        resizable = FALSE)
{
    # Check input
    x <- tidyMatrixValues(x, transpose, row.header.labels, col.header.labels)
    stat <- attr(x, "statistic")
    nrows <- nrow(x)
    ncols <- ncol(x)
    if (is.null(colnames(x)))
        show.col.headers <- FALSE
    if (is.null(rownames(x)))
        show.row.headers <- FALSE
    if (!enable.y.scroll) # all rows are stretched to fit height of window - no scrolling
        num.header.rows <- 0
    if (num.header.rows >= nrows)
        num.header.rows <- nrows - 1

    # Format table contents
    if (isTRUE(grepl("%", stat)))
        x <- x/100
    if (format.type == "Automatic" && any(grepl("%)?$", stat)))
        format.type <- "Percentage"

    content <- if (!is.numeric(x))                   x
               else if (format.type == "Percentage") FormatAsPercent(x, decimals = format.decimals, show.sign = format.show.pct.sign)
               else                                  FormatAsReal(x, decimals = format.decimals)
    content <- matrix(paste0(cell.prefix, content, cell.suffix), nrows, ncols)
    if (suppress.nan)
        content[which(is.nan(x))] <- "<br>"
    if (suppress.na)
        content[which(is.na(x) & !is.nan(x))] <- "<br>"
    ind.empty <- which(!nzchar(content))
    if (any(ind.empty))
        content[ind.empty] <- "<br>"
    if (is.character(x))
    {
        # check image tags and remove and warn for invalid urls
        # wrap images in a div to preserve alignment
        ind <- grep("<img", x, fixed = TRUE)
        for (ii in ind)
            content[ii] <- checkImageTag(content[ii])
    }

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

        # Unfilled leader circles
        circle.css <- paste0('.circle2 {  border: ', metric.leader.border, ';', circle.fmt, '}\n',
                             '.circle1 {  border: ', metric.tie.border, ';', circle.fmt, '}\n',
                             '.circle0 {  border: 0px solid rgb(0,0,0);', circle.fmt, '}\n')

        # CSS generation for filled leader circles
        circle.types = paste0(rep(c(2, 1, 0), 3), rep(c(1,0,-1), each=3))
        circle.colors = rep(c(sig.fills.up, sig.fills.nothing, sig.fills.down), each=3)
        circle.border = rep(c(metric.leader.border, metric.tie.border, '0px solid rgb(0,0,0)'), 3)
        filled.circle.styles = paste0('.circle', circle.types,' { border: ', circle.border,';
                               background-color:',circle.colors,';', circle.fmt, '}', collapse='  ')
        circle.css <- paste0(circle.css, filled.circle.styles)
        sig.leader.circles[!which(sig.leader.circles == 1 | sig.leader.circles == 2)] <- 0
        content <- matrix(sprintf('<div class="circle%s">%s</div>', sig.leader.circles, content), nrows, ncols)
    }
    if (!banded.rows && !banded.cols)
        cell.fill <- matrix(rep(paste("background:", cell.fill, ";"), length = nrows * ncols), nrows, ncols)
    else
        cell.fill <- matrix("", nrows, ncols)

    # Significance coloring takes precedence over cell.fill or class definitions
    # At the moment only sig.change.fills affects cell.inline.style
    cell.inline.style <- matrix("", nrows, ncols)
    if (!is.null(sig.change.fills))
    {
        cell.inline.style[which(sig.change.fills ==  1)] <- paste0(" style='background:", sig.fills.up, "'")
        cell.inline.style[which(sig.change.fills == -1)] <- paste0(" style='background:", sig.fills.down, "'")
    }
    if (show.row.headers)
        cell.inline.style <- cbind("", cell.inline.style)
    if (show.col.headers)
        cell.inline.styl <- rbind("", cell.inline.style)
    override.borders <- grepl("border", custom.css, fixed = TRUE) && grepl("nth-child", custom.css, fixed = TRUE)

    # Setup html file
    tfile <- createTempFile()
    cata <- createCata(tfile)
    # Create unique class name for parent div container
    container.name <- paste0("custom-table-container-", generateRandomString())
    container.selector.name <- paste0(".", container.name)
    cata("<style>\n")
    if (is.numeric(border.row.gap))
        border.row.gap <- paste0(border.row.gap, "px")
    if (is.numeric(border.column.gap))
        border.column.gap <- paste0(border.column.gap, "px")
    cata(container.selector.name, "{ table-layout: fixed; border-collapse: ",
         if (border.collapse) "collapse; " else "separate; ",
         "border-spacing: ", border.column.gap, border.row.gap, ";",
         "position: relative; width: 100%; ",
         "font-family: ", global.font.family, "; color: ", global.font.color, "; ",
         "cellspacing:'0'; cellpadding:'0'; ",
         "white-space: normal; line-height: normal; }\n")

    # Sticky only applies to <th> elements inside <thead> - i.e. column headers not row headers
    # Both the height and position are defined inside cell.styles/row.header.styles
    # to allow for multiple sticky rows
    if (show.col.headers)
        cata(container.selector.name, "th { position: -webkit-sticky; position: sticky; top:",
            paste0("-", 0 + col.header.border.width, "px;"), "overflow: ", overflow, "; ")
    if (resizable)
        cata("resize: both; ")
    cata("}\n")
    cata(container.selector.name, "td { overflow: ", overflow, "; ")
    if (sum(nchar(row.height)) > 0)
        cata("height:", row.height, "; ")
    cata("}\n")

    # supply units if none given (default px); however other units such as pt, em still valid
    if (!show.col.headers)
        col.header.height <- "0px"
    if (length(col.header.height) > 0 && !is.na(suppressWarnings(as.numeric(col.header.height))))
        col.header.height <- paste0(col.header.height, "px")

    # initialize positions for sticky header with scrollable table
    top.position <- NULL
    if (enable.y.scroll && num.header.rows > 0)
    {
        top.position <- sprintf("%s + %.0fpx", col.header.height, col.header.border.width)
        if (num.header.rows > 1)
        {
            join.str <- sprintf(" + %.0fpx + ", cell.border.width)
            hh <- c(top.position, rep(row.height, num.header.rows - 1))
            top.position <- paste0("calc(", sapply(1:num.header.rows,
                function(i) paste(rep(hh, length = i), collapse = join.str)), ")")
        }
    }

    # Set up styles for each cell
    ncells <- nrows * ncols
    cell.styles <- addCSSclass(cata, "celldefault",
        rep(paste0(cell.fill, "; ", if (sum(nchar(row.height)) > 0) paste0("height: ", row.height, "; ") else "",
        if (override.borders) "" else paste0("border: ", cell.border.width, "px solid ", cell.border.color),
        ";", getPaddingCSS(tolower(cell.align.horizontal), cell.pad),
        "; font-size: ", cell.font.size, font.unit, "; font-style: ", cell.font.style,
        "; font-weight: ", cell.font.weight, "; font-family: ", cell.font.family,
        "; color:", cell.font.color, "; text-align: ", cell.align.horizontal,
        "; vertical-align: ", cell.align.vertical, ";"), length=ncells), nrows, ncols,
        position = top.position, parent.stem = container.name)

    # Row/column classes overrides other attributes (except coloring based on significance)
    for (cc in row.classes)
        cell.styles[cc[[1]],] = paste(cell.styles[cc[[1]],], cc[[2]])

    for (cc in col.classes)
        cell.styles[,cc[[1]]] <- paste(cell.styles[,cc[[1]]], cc[[2]])

    # Row headers
    row.header.class.css <- NULL
    if (show.row.headers)
    {
        row.header.styles <- addCSSclass(cata, "rowheaderdefault", paste0("background: ", row.header.fill,
            if (override.borders) "" else paste0("; border: ", row.header.border.width, "px solid ", row.header.border.color),
            ";", getPaddingCSS(tolower(row.header.align.horizontal), row.header.pad),
            "; font-size: ", row.header.font.size, font.unit, "; font-style: ", row.header.font.style,
            "; font-weight: ", row.header.font.weight, "; font-family: ", row.header.font.family,
            "; color:", row.header.font.color, "; text-align: ", row.header.align.horizontal,
            "; vertical-align: ", row.header.align.vertical, ";"), nrows, position = top.position,
            parent.stem = container.name)
        if (!is.null(row.header.classes))
            row.header.styles <- paste(row.header.styles, row.header.classes)
        content <- cbind(rownames(x), content)
        cell.styles <- cbind(row.header.styles, cell.styles)
    } else { corner = NULL; corner.class = NULL; }

    # Row spans
    row.span.class.css <- NULL
    if (!is.null(row.spans))
    {
        if (!is.null(top.position))
        {
            j <- 1
            rm.index <- c()
            for (i in 1:length(row.spans))
            {
                offset <- row.spans[[i]]$height - 1
                if (offset >= 1)
                    rm.index <- c(rm.index, j + (1:offset))
                j <- j + offset + 1
            }
            top.position <- top.position[-rm.index]

        }

        row.span.lengths <- sapply(row.spans, function(x) x[['height']])
        row.span.styles <- addCSSclass(cata, "rowspandefault", paste0("background: ", row.span.fill,
            if (override.borders) "" else paste0("; border: ", row.span.border.width,
            "px solid ", row.span.border.color),
            ";", getPaddingCSS(tolower(row.span.align.horizontal), row.span.pad),
            "; font-size: ", row.span.font.size, font.unit, "; font-style: ", row.span.font.style,
            "; font-weight: ", row.span.font.weight, "; font-family: ", row.span.font.family,
            "; color:", row.span.font.color, "; text-align: ", row.span.align.horizontal,
            "; vertical-align: ", row.span.align.vertical, ";"), length(row.span.lengths), position = top.position,
            parent.stem = container.name)
        for (i in 1:length(row.spans))
            if (!is.null(row.spans[[i]]$class))
                row.span.styles[i] <- paste(row.span.styles[i], row.spans[[i]]$class)

        row.spans <- sapply(1:length(row.spans), function(i) sprintf('<td rowspan="%s" class="%s">%s</td>',
                            row.spans[[i]][['height']], row.span.styles[i], row.spans[[i]][['label']]))
        row.span.html <- rep("", nrows)
        j <- 1
        for (i in 1:length(row.spans))
        {
            row.span.html[j] <- row.spans[i]
            j <- j + row.span.lengths[i]
        }

    } else
        row.span.html <- ''

    # Column headers
    col.header.class.css <- NULL
    if (show.col.headers)
    {
        col.header.styles <- addCSSclass(cata, "colheaderdefault", paste0("background: ", col.header.fill,
            "; ", if (sum(nchar(col.header.height)) > 0) paste0("height: ", col.header.height, "; ") else "",
            if (override.borders) "" else paste0("; border: ", col.header.border.width,
            "px solid ", col.header.border.color),
            ";", getPaddingCSS(tolower(col.header.align.horizontal), col.header.pad),
            "; font-size: ", col.header.font.size, font.unit, "; font-style: ", col.header.font.style,
            "; font-weight: ", col.header.font.weight, "; font-family: ", col.header.font.family,
            "; color:", col.header.font.color, "; text-align: ", col.header.align.horizontal,
            "; vertical-align: ", col.header.align.vertical, ";"), ncols,
            parent.stem = container.name)
        if (!is.null(col.header.classes))
            col.header.styles <- paste(col.header.styles, col.header.classes)
        col.labels <- colnames(x)

        if (show.row.headers)
        {
            corner.styles <- addCSSclass(cata, "cornerdefault",
                paste0("background: ", corner.fill,
                if (override.borders) "" else paste0("; border: ", corner.border.width, "px solid ", corner.border.color),
                ";", getPaddingCSS(tolower(corner.align.horizontal), corner.pad),
                "; font-size: ", corner.font.size, font.unit, "; font-style: ", corner.font.style,
                "; font-weight: ", corner.font.weight, "; font-family: ", corner.font.family,
                "; color:", corner.font.color, "; text-align: ", corner.align.horizontal,
                "; vertical-align: ", corner.align.vertical, ";"),
                parent.stem = container.name)
            if (sum(nchar(corner.class)) > 0)
                corner.styles <- paste(corner.styles, corner.class)
            col.header.styles <- c(corner.styles[1], col.header.styles)
            col.labels <- c(corner, col.labels)
        }

        if (!is.null(row.spans))
        {
            col.header.styles <- c(corner.styles[1], col.header.styles)
            col.labels <- c("", col.labels)
        }
        if (!is.null(spacer.col))
            col.header.styles[spacer.col] <- "spacer"
        if (!is.null(spacer.row))
            spacer.row <-  spacer.row + 1
        header.html <- paste0(c('<tr>', sprintf('<th class="%s">%s</th>', col.header.styles, col.labels),
                                '</tr>'), collapse='')
    } else
        header.html <- ''

    # Column spans
    if (!is.null(col.spans))
    {
        col.span.lengths <- sapply(col.spans, function(x) x[['width']])
        col.span.styles <- addCSSclass(cata, "colspandefault", paste0("background: ", col.span.fill,
            if (override.borders) "" else paste0("; border: ", col.span.border.width,
            "px solid ", col.span.border.color),
            ";", getPaddingCSS(tolower(col.span.align.horizontal), col.span.pad),
            "; font-size: ", col.span.font.size, font.unit, "; font-style: ", col.span.font.style,
            "; font-weight: ", col.span.font.weight, "; font-family: ", col.span.font.family,
            "; color:", col.span.font.color, "; text-align: ", col.span.align.horizontal,
            "; vertical-align: ", col.span.align.vertical, ";"), length(col.span.lengths), position = top.position,
            parent.stem = container.name)
        for (i in 1:length(col.spans))
            if (!is.null(col.spans[[i]]$class))
                col.span.styles[i] <- paste(col.span.styles[i], col.spans[[i]]$class)

        col.spans <- sapply(1:length(col.spans), function(i) sprintf('<th colspan="%s" class="%s">%s</th>',
                            col.spans[[i]][['width']], col.span.styles[i], col.spans[[i]][['label']]))
        col.span.html <- paste0('<tr>', paste0(col.spans, collapse=''),'</tr>')
    } else
        col.span.html <- ''


    # Row/Column banding
    if (banded.rows)
        cata(container.selector.name, 'tbody tr:nth-child(odd){background-color:', banded.odd.fill,
                ';} tr:nth-child(even){background-color:', banded.even.fill, ';}')
    if (banded.cols)
        cata(container.selector.name, 'tbody td:nth-child(2n+3){background-color:', banded.odd.fill,
             ';} td:nth-child(even){background-color:', banded.even.fill, ';}')

    # Scrollbars
    enable.scroll <- enable.x.scroll || enable.y.scroll
    if (enable.scroll)
    {
        y.scroll <- if (enable.y.scroll) "auto" else "hidden"
        x.scroll <- if (enable.x.scroll) "auto" else "hidden"
        cata("\ndiv { position: absolute; overflow-y:", y.scroll, "; overflow-x:", x.scroll, "; }\n")
    }

    # Other CSS
    if (use.predefined.css)
        cata("\n", predefinedCSS(container.selector.name), "\n")
    cata("\n", circle.css, "\n")
    cata("\n", custom.css, "\n")
    cata("</style>\n\n")

    table.height <- if (sum(nchar(row.height)) != 0) ""
                    else paste0("; height:calc(100% - ", rev(cell.border.width)[1], "px)")
    cata(sprintf("<table class = '%s' style = 'width:calc(%s - %dpx)%s'>\n",
        container.name, "100%", max(0, max(cell.border.width)), table.height))
    if (sum(nchar(col.widths)) > 0)
    {
        col.widths <- ConvertCommaSeparatedStringToVector(col.widths)
        cata(paste(paste("<col width='", col.widths, "'>\n"), collapse = ""))
    }
    cata('<thead>', col.span.html, header.html)

    # Build table
    cell.html <- matrix(sprintf('<td class="%s"%s>%s</td>', cell.styles, cell.inline.style, content),
                        nrow = nrows)
    cell.html <- cbind(row.span.html, cell.html)

    if (num.header.rows > 0) # additional rows that float at the top
    {
        extra.header.html <- paste0(sprintf('<tr>%s</tr>\n',
                                apply(cell.html[1:num.header.rows,,drop = FALSE], 1,
                                paste0, collapse = '')), collapse='')
        extra.header.html <- gsub("<td ", "<th ", extra.header.html, fixed = TRUE)
        extra.header.html <- gsub("</td>", "</th>", extra.header.html, fixed = TRUE)
        cata(extra.header.html)
        cell.html <- cell.html[-(1:num.header.rows),,drop = FALSE]
    }
    cata('</thead>')
    body.html <- paste0(sprintf('<tr>%s</tr>\n',
                    apply(cell.html, 1, paste0, collapse = '')), collapse='')
    cata(body.html)

    # Optional footer
    if (nchar(footer) > 0)
    {
        tot.columns <- (ncols + show.row.headers + !is.null(row.spans))
        cata(paste0('<tr><th colspan="', tot.columns, '" style="',
            'height:', footer.height,
            '; line-height:', footer.lineheight,
            '; background-color:', footer.fill,
            '; font-family:', footer.font.family,
            '; color:', footer.font.color,
            '; font-size:', footer.font.size, font.unit,
            '; font-style:', footer.font.style,
            '; font-weight:', footer.font.weight,
            '; text-align:', footer.align.horizontal,
            '; vertical-align:', footer.align.vertical,
            '">', footer, '</th></tr>\n'))
    }
    cata("</table>\n")
    html <- paste(readLines(tfile), collapse = "\n")
    if (enable.scroll || !any(nzchar(custom.css)))
        out <- boxIframeless(html, text.as.html = TRUE,
                         font.family = "Circular, Arial, sans-serif",
                         font.size = 8)
    else
        out <- Box(html, text.as.html = TRUE,
                         font.family = "Circular, Arial, sans-serif",
                         font.size = 8)
    class(out) <- c(class(out), "visualization-selector")
    attr(out, "ChartData") <- prepareForExport(x, format.type)
    return(out)
}

prepareForExport <- function(x, format.type)
{
    if (format.type == "Percentage")
    {
        x <- x * 100
        attr(x, "statistic") <- "%"
        return(x)
    }
    else if (is.numeric(x))
        return(x)
    else
        return(clean_html(x))

}


#' @importFrom xml2 xml_text read_xml
clean_html <- function(x)
{
    if (!is.character(x))
        return(x)

    .strip_html <- function(x) if (!nzchar(trimws(x))) x else xml_text(read_xml(charToRaw(x), as_html = TRUE))
    if (is.matrix(x))
        return(apply(x, c(1, 2), .strip_html))
    else
        return(sapply(x, .strip_html))
}

tidyMatrixValues <- function(x, transpose, row.header.labels, col.header.labels)
{
    stat <- attr(x, "statistic")
    ndim <- length(dim(x))

    # extract primary statistic from higher dimensions if x is a QTable
    if (is.null(stat) && all(c("questions", "name") %in% names(attributes)))
        stat <- dimnames(x)[[ndim]][1]
    if (ndim == 3)
        x <- x[,,1]
    if (ndim == 4)
        x <- x[,,1,1]

    x <- as.matrix(x)
    if (transpose)
        x <- t(x)

    if (length(row.header.labels) < nrow(x))
        row.header.labels <- ConvertCommaSeparatedStringToVector(row.header.labels)
    if (sum(nchar(row.header.labels)) > 0)
    {
        new.labels <- paste0(rownames(x), rep("", nrow(x))) # in case rownames is NULL
        tmp.len <- min(length(row.header.labels), length(new.labels))
        new.labels[1:tmp.len] <- row.header.labels[1:tmp.len]
        rownames(x) <- new.labels
    }
    if (length(col.header.labels) < ncol(x))
        col.header.labels <- ConvertCommaSeparatedStringToVector(col.header.labels)
    if (sum(nchar(col.header.labels)) > 0)
    {
        new.labels <- paste0(colnames(x), rep("", ncol(x)))
        tmp.len <- min(length(col.header.labels), length(new.labels))
        new.labels[1:tmp.len] <- col.header.labels[1:tmp.len]
        colnames(x) <- new.labels
    }
    attr(x, "statistic") <- stat
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

addCSSclass <- function(cata, class.stem, class.css, nrow = 1, ncol = 1, position = NULL, parent.stem = NULL)
{
    if (length(class.css) < 1)
        return(NULL)
    if (!is.null(position))
    {
        class.css <- matrix(rep(class.css, length = nrow * ncol), nrow, ncol)
        for (i in 1:length(position))
            class.css[i,] <- paste0("position: sticky; top: ", position[i], "; ", class.css[i,])
    }
    n <- length(class.css)

    # The number of classes created is the length of class.css
    # recycling occurs if needed inside CreateCustomTable
    class.names <- paste0(class.stem, 1:n)
    css.selectors <- if (!is.null(parent.stem)) paste0(".", parent.stem, " .", class.names) else paste0(".", class.names)
    tmp.css <- paste0(css.selectors, "{ ", class.css, " }")

    # Add class definition to CSS file
    cata(paste(tmp.css, collapse = "\n"))

    # Return class names - otherwise the main function does not know
    # how many classes were created
    if (ncol == 1)
        return(rep(class.names, length = nrow))
    else
        return(matrix(rep(class.names, length = nrow * ncol), nrow, ncol))
}



predefinedCSS <- function(container.selector.name)
{
    do.call(sprintf, as.list(c("
    %s .rh {
        text-align:left;
        font-weight: bold;
        }
    %s .rhclean {
        text-align:left;
        }
    %s .simpleheader {
        background: #DCDCDC;
        font-weight: bold;
        }
    %s .simpleheaderclean {
        background: #DCDCDC;
        font-weight: normal;
        }
    %s .nsline {
        font-style: italic;
        font-size: 9pt;
        white-space:nowrap;
        display: block;}
    %s .subjourneyHeader{
        font-style: bold;
        border-bottom: 1px black solid;
        }
    %s .subjourneySubHeader{
        font-style: bold;
        border-top: 1px grey solid;
        border-bottom: 1px grey solid;
        }
    %s .white {background-color:white;}
    %s .spacer {background: white;color: white;border: none;overflow:hidden;}
", rep(container.selector.name, 9))))

}
