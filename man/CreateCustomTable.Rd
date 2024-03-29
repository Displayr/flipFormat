% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createcustomtable.R
\name{CreateCustomTable}
\alias{CreateCustomTable}
\title{Display a html table with custom formatting}
\usage{
CreateCustomTable(
  x,
  sig.change.fills = NULL,
  sig.change.arrows = NULL,
  sig.leader.circles = NULL,
  format.type = "Automatic",
  format.show.pct.sign = TRUE,
  format.decimals = 0,
  suppress.nan = TRUE,
  suppress.na = TRUE,
  transpose = FALSE,
  col.widths = if (is.null(rownames(x))) NULL else c("25\%"),
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
  banded.odd.fill = "rgb(250,250,250)",
  banded.even.fill = "rgb(245,245,245)",
  sig.fills.up = "rgb(195,255,199)",
  sig.fills.down = "rgb(255,213,213)",
  sig.fills.nothing = "rgb(255,255,255)",
  sig.arrows.up = "rgb(0,172,62)",
  sig.arrows.down = "rgb(192,0,0)",
  circle.size = 35,
  spacer.row = NULL,
  spacer.col = NULL,
  col.spans = NULL,
  row.spans = NULL,
  overflow = "hidden",
  custom.css = "",
  use.predefined.css = TRUE,
  resizable = FALSE
)
}
\arguments{
\item{x}{Matrix or data frame of contents to show in the table}

\item{sig.change.fills}{Matrix of same dim as \code{x} used for cell fills
(1 denotes increase/green fill, -1 denotes decrease/red fill, 0 no fill)}

\item{sig.change.arrows}{Matrix of same dim as \code{x} used for cell arrows
(1 denotes increase/green up arrow, -1 denotes decrease/red down arrow, 0 no arrow)}

\item{sig.leader.circles}{Matrix of same dim as \code{x} used for 'leader' circles
(2 denotes row leader, 1 denotes tied leaders, 0 denotes no circle)}

\item{format.type}{One of "Automatic", "Percentage (multiply by 100
and add percentage sign) or "Numeric". When set to "Automatic", the format type
will be determined by \code{attr(x, "statistic")}. Ignored if \code{x} is not numeric.}

\item{format.show.pct.sign}{Show percentage sign when \code{format.type} is "Percentage".}

\item{format.decimals}{Controls number of decimal places shown in table cells.
Ignored if \code{x} is not numeric.}

\item{suppress.nan}{whether to empty cells containing only NaN}

\item{suppress.na}{whether to empty cells containing only NA}

\item{transpose}{Whether to switch rows and columns in \code{x}.}

\item{col.widths}{specify column widths in \% or px; Remaining width divided between remaining columns.}

\item{row.height}{Height of table body rows. If \code{NULL}, then the rows are stretched to fill container.}

\item{col.header.height}{Height of table header rows}

\item{num.header.rows}{This is the number of rows from \code{x} which always be shown at the
top of the window (only used when \code{row.height} is specified.}

\item{global.font.family}{Character; font family for all occurrences of any
font attribute in the table unless specified individually.}

\item{global.font.color}{Global font color as a named color in character format
(e.g. "black") or an a hex code.}

\item{font.size}{Global font size of all elements in the table. This is provided for
convenience but its overriden by the font sizes of specific components.}

\item{font.unit}{One of "px" of "pt". By default all font sizes are specified in terms of
pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
points ("pt"), which will be consistent with font sizes in text boxes.}

\item{border.color}{Color of all borders. Will be overriden if specific elements are set.}

\item{border.width}{Width of borders (in pixels) in all cells. Will be overriden if specific elements are set.}

\item{border.collapse}{Logical; whether the borders of adjacent cells
should be shown as a single line or separate lines.}

\item{border.row.gap}{Numeric; the space between the borders
separating different rows. Only used if \code{border.collapse} is false.}

\item{border.column.gap}{Numeric; the space between the borders
separating different columns. Only used if \code{border.collapse} is false.}

\item{cell.prefix}{Character value/vector/matrix that is prepended before the cell values.}

\item{cell.suffix}{Character value/vector/matrix that is appended after the cell values.}

\item{cell.fill}{Background color of the cells in the table.}

\item{cell.border.width}{Width of border around table cells (in pixels).}

\item{cell.border.color}{Color of border around table cells,}

\item{cell.align.horizontal}{Horizontal alignment of text in table cells.}

\item{cell.align.vertical}{Vertical alignment of text in table cells.}

\item{cell.font.family}{Font family of text in table cells.}

\item{cell.font.color}{Font color of text in table cells.}

\item{cell.font.size}{Font size (in pixels) of text in table cells.}

\item{cell.font.weight}{One of "normal" or "bold".}

\item{cell.font.style}{One of "normal" or "italic".}

\item{cell.pad}{Space between text and cell border in pixels. This is only used if the
horizontal alignment is "left" or "right".}

\item{show.col.headers}{Logical; whether to show column headers in the table.
This will be ignored if \code{x} does not contain column names.}

\item{col.header.labels}{A vector or comma-separated labels to override the
column names of \code{x}.}

\item{col.header.fill}{Background color of the column headers in the table.}

\item{col.header.border.width}{Width of border around table column headers (in pixels).}

\item{col.header.border.color}{Color of border around table column headers,}

\item{col.header.align.horizontal}{Horizontal alignment of text in table column headers.}

\item{col.header.align.vertical}{Vertical alignment of text in table column headers.}

\item{col.header.font.family}{Font family of text in table column headers.}

\item{col.header.font.color}{Font color of text in table column headers.}

\item{col.header.font.size}{Font size (in pixels) of text in table column headers.}

\item{col.header.font.weight}{One of "normal" or "bold".}

\item{col.header.font.style}{One of "normal" or "italic".}

\item{col.header.pad}{Space between text and cell border in pixels. This is only used if the
horizontal alignment is "left" or "right".}

\item{show.row.headers}{Logical; whether to show row headers in the table.
This will be ignored if \code{x} does not contain row names.}

\item{row.header.labels}{A vector or comma-separated labels to override
the column names of \code{x}.}

\item{row.header.fill}{Background color of the row headers in the table.}

\item{row.header.border.width}{Width of border around table row headers (in pixels).}

\item{row.header.border.color}{Color of border around table row headers,}

\item{row.header.align.horizontal}{Horizontal alignment of text in table row headers.}

\item{row.header.align.vertical}{Vertical alignment of text in table row headers.}

\item{row.header.font.family}{Font family of text in table row headers.}

\item{row.header.font.color}{Font color of text in table row headers.}

\item{row.header.font.size}{Font size (in pixels) of text in table row headers.}

\item{row.header.font.style}{One of "normal" or "italic".}

\item{row.header.font.weight}{One of "normal" or "bold".}

\item{row.header.pad}{Space between text and cell border in pixels. This is only used if the
horizontal alignment is "left" or "right".}

\item{row.span.fill}{Background color of the row.spans in the table.}

\item{row.span.border.width}{Width of border around table row.spans (in pixels).}

\item{row.span.border.color}{Color of border around table row.spans,}

\item{row.span.align.horizontal}{Horizontal alignment of text in table row.spans.}

\item{row.span.align.vertical}{Vertical alignment of text in table row.spans.}

\item{row.span.font.family}{Font family of text in table row.spans.}

\item{row.span.font.color}{Font color of text in table row.spans.}

\item{row.span.font.size}{Font size (in pixels) of text in table row.spans.}

\item{row.span.font.style}{One of "normal" or "italic".}

\item{row.span.font.weight}{One of "normal" or "bold".}

\item{row.span.pad}{Space between text and cell border in pixels. This is only used if the
horizontal alignment is "left" or "right".}

\item{col.span.fill}{Background color of the col.spans in the table.}

\item{col.span.border.width}{Width of border around table col.spans (in pixels).}

\item{col.span.border.color}{Color of border around table col.spans,}

\item{col.span.align.horizontal}{Horizontal alignment of text in table col.spans.}

\item{col.span.align.vertical}{Vertical alignment of text in table col.spans.}

\item{col.span.font.family}{Font family of text in table col.spans.}

\item{col.span.font.color}{Font color of text in table col.spans.}

\item{col.span.font.size}{Font size (in pixels) of text in table col.spans.}

\item{col.span.font.style}{One of "normal" or "italic".}

\item{col.span.font.weight}{One of "normal" or "bold".}

\item{col.span.pad}{Space between text and cell border in pixels. This is only used if the
horizontal alignment is "left" or "right".}

\item{corner}{Contents of the corner cell, if row and column headers are used}

\item{corner.class}{Class of the corner cell, if row and column headers are used}

\item{corner.fill}{Background color of the corners in the table.}

\item{corner.border.width}{Width of border around table corners (in pixels).}

\item{corner.border.color}{Color of border around table corners,}

\item{corner.align.horizontal}{Horizontal alignment of text in table corners.}

\item{corner.align.vertical}{Vertical alignment of text in table corners.}

\item{corner.font.family}{Font family of text in table corners.}

\item{corner.font.color}{Font color of text in table corners.}

\item{corner.font.size}{Font size (in pixels) of text in table corners.}

\item{corner.font.weight}{One of "normal" or "bold".}

\item{corner.font.style}{One of "normal" or "italic".}

\item{corner.pad}{Space between text and cell border in pixels. This is only used if the
horizontal alignment is "left" or "right".}

\item{footer}{Optional text shown as a footer below the table}

\item{footer.height}{Height of the footer (ignored if no text in footer).}

\item{footer.lineheight}{Controls spacing between the lines of text in the
footer. It can be specified in multiple ways but as a unitless number
it is applied as a multiple to the font size.}

\item{footer.fill}{Background color of the footer in the table.}

\item{footer.align.horizontal}{Horizontal alignment of text in table footer.}

\item{footer.align.vertical}{Vertical alignment of text in table footer.}

\item{footer.font.family}{Font family of text in table footer.}

\item{footer.font.color}{Font color of text in table footer.}

\item{footer.font.size}{Font size of text in table footer.}

\item{footer.font.weight}{One of "normal" or "bold".}

\item{footer.font.style}{One of "normal" or "italic".}

\item{col.header.classes}{CSS classes of column headers. The class definition should be added to
\code{custom.css}. This overrides \code{col.header.fill},
\code{col.header.border}, \code{col.header.font}, \code{col.header.align}, etc.}

\item{row.header.classes}{CSS classes of column headers. This overrides \code{row.header.fill},
\code{row.header.border}, \code{row.header.font}, \code{row.header.align}, etc}

\item{col.classes}{any specific column classes to apply. e.g. \code{list(list(ix=3, class="bluefill"))}
will cause column 3 to have class "bluefill".}

\item{row.classes}{any specific row classes to apply.}

\item{banded.rows}{Whether to have banded rows}

\item{banded.cols}{Whether to have banded columns}

\item{banded.odd.fill}{Background of cells in odd rows or columns when \code{banded.rows} or \code{banded.cols}.}

\item{banded.even.fill}{Background of cells in even rows or columns when \code{banded.rows} or \code{banded.cols}.}

\item{sig.fills.up}{Cell color when \code{sig.change.fills} is used.}

\item{sig.fills.down}{Cell color when \code{sig.change.fills} is used.}

\item{sig.fills.nothing}{Cell color when \code{sig.change.nothing} is used.}

\item{sig.arrows.up}{Color of up arrows when \code{sig.change.arrows} is used.}

\item{sig.arrows.down}{Color of down arrows when \code{sig.change.arrows} is used.}

\item{circle.size}{Size of circles when \code{sig.leader.circles} is used.}

\item{spacer.row}{Indices of any blank divider rows}

\item{spacer.col}{Indices of any blank divider columns}

\item{col.spans}{List of column spans to place above the column headers:
list(list(width=,label=,class=), list(width=,label=,class=))}

\item{row.spans}{List of row spans to place left of the row headers: list(list(height=,label=,class=),
list(height=,label=,class=)}

\item{overflow}{Determines behaviour of text that is too long to fit in the table cells. By default,
it is set to "hidden" but change to "visible" to show overflow text.}

\item{custom.css}{Any custom CSS to add to the \code{<style>} header of the html
(e.g. defining nth-child logic or custom classes not included in the CSS function).
When this is used, the resulting widget is inclosed inside an iframe to avoid
affecting other widgets.}

\item{use.predefined.css}{Logical; whether to include CSS definitions for classes
\code{rh, rhclean, simpleheader, simpleheaderclean, nsline, subjourneyHeader, subjourneySubHeader
white, spacer}. This is included for backwards compatibiliy but it is probably safer
to omit this is not used.}

\item{resizable}{Allow column widths to be resizeable by dragging with mouse.}
}
\description{
Displays html table with custom formatting. This can be specified
 separately for each cell be specifying attributes or using CSS.
}
\examples{
xx <- structure(1:24, .Dim = c(4L, 6L), .Dimnames = list(c("a", "b", "c", "d"),
         c("A", "B", "C", "D", "E", "F")))
CreateCustomTable(xx, row.spans=list(list(height=2, label="AA"),
         list(height=1, label="BB"), list(height=1, label="CC")))
}
