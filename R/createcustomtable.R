#' Display a table with custom formatting
#' @description Displays a table which fills up the entire containiner with
#'  resizable column widths and row heights.
#' @param x Input table to display.
#' @param format.as.percentage Display table values as percentage (multiply by 100
#'  and add percentage sign). Ignored if \code{x} is not numeric.
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
#' @param values.fill Background color of the cells in the table.
#' @param values.border.width Width of border around table cells (in pixels).
#' @param values.border.color Color of border around table cells,
#' @param values.align.horizontal Horizontal alignment of text in table cells.
#' @param values.align.vertical Vertical alignment of text in table cells.
#' @param values.font.family Font family of text in table cells.
#' @param values.font.color Font color of text in table cells.
#' @param values.font.size Font size (in pixels) of text in table cells.
#' @param values.font.weight One of "normal" or "bold".
#' @param values.font.style One of "normal" or "italic".
#' @param colhead.show Logical; whether to show column headers in the table.
#'  This will be ignored if \code{x} does not contain column names.
#' @param colhead.labels A vector or comma-separated labels to override the
#'  column names of \code{x}.
#' @param colhead.fill Background color of the column headers in the table.
#' @param colhead.border.width Width of border around table column headers (in pixels).
#' @param colhead.border.color Color of border around table column headers,
#' @param colhead.align.horizontal Horizontal alignment of text in table column headers.
#' @param colhead.align.vertical Vertical alignment of text in table column headers.
#' @param colhead.font.family Font family of text in table column headers.
#' @param colhead.font.color Font color of text in table column headers.
#' @param colhead.font.size Font size (in pixels) of text in table column headers.
#' @param colhead.font.weight One of "normal" or "bold".
#' @param colhead.font.style One of "normal" or "italic".
#' @param rowhead.show Logical; whether to show row headers in the table.
#'  This will be ignored if \code{x} does not contain row names.
#' @param rowhead.labels A vector or comma-separated labels to override
#'   the column names of \code{x}.
#' @param rowhead.fill Background color of the row headers in the table.
#' @param rowhead.border.width Width of border around table row headers (in pixels).
#' @param rowhead.border.color Color of border around table row headers,
#' @param rowhead.align.horizontal Horizontal alignment of text in table row headers.
#' @param rowhead.align.vertical Vertical alignment of text in table row headers.
#' @param rowhead.font.family Font family of text in table row headers.
#' @param rowhead.font.color Font color of text in table row headers.
#' @param rowhead.font.size Font size (in pixels) of text in table row headers.
#' @param rowhead.font.weight One of "normal" or "bold".
#' @param rowhead.font.style One of "normal" or "italic".
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @export
CreateCustomTable <- function(x,
                              format.as.percentage = FALSE,
                              format.decimals = 0,
                              transpose = FALSE,
                              global.font.family = "Arial",
                              global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                              font.unit = "pt",
                              values.fill = "transparent",
                              values.border.width = 1,
                              values.border.color = "#FFFFFF",
                              values.align.horizontal = "center",
                              values.align.vertical = "middle",
                              values.font.family = global.font.family,
                              values.font.color = global.font.color,
                              values.font.size = 10,
                              values.font.weight = "normal",
                              values.font.style = "normal",
                              colhead.show = TRUE,
                              colhead.labels = NULL,
                              colhead.fill = "#DDDDDD",
                              colhead.border.width = 1,
                              colhead.border.color = "#FFFFFF",
                              colhead.align.horizontal = "center",
                              colhead.align.vertical = "middle",
                              colhead.font.family = global.font.family,
                              colhead.font.color = global.font.color,
                              colhead.font.size = 10,
                              colhead.font.weight = "bold",
                              colhead.font.style = "normal",
                              rowhead.show = TRUE,
                              rowhead.labels = NULL,
                              rowhead.fill = colhead.fill,
                              rowhead.border.width = 1,
                              rowhead.border.color = "#FFFFFF",
                              rowhead.align.horizontal = "left",
                              rowhead.align.vertical = "middle",
                              rowhead.font.family = global.font.family,
                              rowhead.font.color = global.font.color,
                              rowhead.font.size = 10,
                              rowhead.font.style = "normal",
                              rowhead.font.weight = "bold")
{
    x <- as.matrix(x)
    if (transpose)
        x <- t(x)
    x.formatted <- if (!is.numeric(x))            x
                   else if (format.as.percentage) FormatAsPercent(x, decimals = format.decimals)
                   else                           FormatAsReal(x, decimals = format.decimals)
    rowhead.labels <- ConvertCommaSeparatedStringToVector(rowhead.labels)
    if (length(rowhead.labels) > 0)
    {
        if (length(rowhead.labels) != nrow(x))
            stop("Row labels (", length(rowhead.labels), ") do not equal the number or rows (",
            nrow(x), ") in the input table")
        rownames(x) <- rowhead.labels
    }
    colhead.labels <- ConvertCommaSeparatedStringToVector(colhead.labels)
    if (length(colhead.labels) > 0)
    {
        if (length(colhead.labels) != ncol(x))
            stop("Column labels (", length(colhead.labels), ") do not equal the number or columns (",
            ncol(x), ") in the input table")
        colnames(x) <- colhead.labels
    }
    if (is.null(colnames(x)))
        colhead.show <- FALSE
    if (is.null(rownames(x)))
        rowhead.show <- FALSE

    # Set up formatting
    if (tolower(font.unit) %in% c("pt", "point", "points"))
    {
        fsc <- 1.3333
        values.font.size = round(fsc * values.font.size, 0)
        colhead.font.size = round(fsc * colhead.font.size, 0)
        rowhead.font.size = round(fsc * rowhead.font.size, 0)
    }

    rowhead.style <- paste0("background: ", rowhead.fill,
        "; border: ", rowhead.border.width, "px solid ", rowhead.border.color,
        "; font-size: ", rowhead.font.size, "px; font-style: ", rowhead.font.style,
        "; font-weight: ", rowhead.font.weight, "; font-family: ", rowhead.font.family,
        "; color:", rowhead.font.color, "; text-align: ", rowhead.align.horizontal,
        "; vertical-align: ", rowhead.align.vertical, ";")
    colhead.style <- paste0("background: ", colhead.fill,
        "; border: ", colhead.border.width, "px solid ", colhead.border.color,
        "; font-size: ", colhead.font.size, "px; font-style: ", colhead.font.style,
        "; font-weight: ", colhead.font.weight, "; font-family: ", colhead.font.family,
        "; color:", colhead.font.color, "; text-align: ", colhead.align.horizontal,
        "; vertical-align: ", colhead.align.vertical, ";")
    values.style <- paste0("background: ", values.fill,
        "; border: ", values.border.width, "px solid ", values.border.color,
        "; font-size: ", values.font.size, "px; font-style: ", values.font.style,
        "; font-weight: ", values.font.weight, "; font-family: ", values.font.family,
        "; color:", values.font.color, "; text-align: ", values.align.horizontal,
        "; vertical-align: ", values.align.vertical, ";")


    # Create html file
    tfile <- createTempFile()
    cata <- createCata(tfile)
    cata("<style>\n")
    cata("table { border-collapse: collapse; }\n")
    cata("th { resize: both; overflow: auto; ", colhead.style, "}\n")
    cata("td { overflow: auto; ", values.style, "}\n")
    cata("</style>\n\n")

    cata("<table style = 'width:100%; height:100%'>\n")
    if (colhead.show)
    {
        cata("<tr>\n")
        if (rowhead.show)
            cata("<th></th>")
        for (j in 1:NCOL(x))
            cata("<th>", colnames(x)[j], "</th>")
        cata("</tr>\n")
    }
    for (i in 1:NROW(x))
    {
        cata("<tr>\n")
        if (rowhead.show)
            cata("<th style = '", rowhead.style, "'>", rownames(x)[i], "</th>")
        for (j in 1:NCOL(x))
            cata("<td>", x.formatted[i,j], "</td>")
        cata("</tr>\n")
    }
    cata("</table>\n")

    html <- paste(readLines(tfile), collapse = "\n")
    out <- rhtmlMetro::Box(html, text.as.html = TRUE,
                    font.family = "Circular, Arial, sans-serif",
                    font.size = 8)

    attr(out, "ChartData") <- x
    return(out)
}

