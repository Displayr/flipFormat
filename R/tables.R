# Tags from the htmltools package. Not recognised by check().
globalVariables(c("style", "thead", "th", "tr"))

#' \code{DataTableWithRItemFormat}
#'
#' @description Create an HTML widget DataTable which looks like Q's blue table style.
#' Facilitates nicer printing of wrapping text and coefficient matrices.
#'
#' @param dd The \code{data frame} that you want to show as a widget.
#' @param caption Text to be placed beneath the table.
#' @param header.alignments A character vector with one element for each column in
#' \code{dd} which specifies whether the cells of each column should be left or right
#' aligned. Acceptable values are \code{"right"} and \code{"left"}. If you don't
#' specify the alignments then columns that look like text will be left-aligned, and
#' columns that look like numbers will be right-aligned.
#' @param allow.length.change Boolean value that determines whether or not the user is
#' given a menu allowing them to choose the height of the table.
#' @param length.menu A vector of integers specifying the options to show in the menu
#' of table heights.
#' @param page.length An integer specifying the initial height of the table.
#' @param allow.paging A boolean value to specify whether pagination is turned on.
#' @param show.info A boolean value to specify whether or not extra info is shown below
#' the table, including pagination info like "Showing 5 of 10 items".
#' @param escape.html Either a boolean value or a vector of integers which determines how
#' to escape html characters in the table. If TRUE, then all columns are escaped, and so
#' no html code will be executed. If FALSE, then no columns will be escaped. A vector
#' of positive integers allows you to specify the columns to be escaped. A vector of
#' negative integers allows you to specify which columns not to escape.
#'
#' @examples
#' my.df <- data.frame(First = c(1,2,3), Second = c("a", "b", "c"))
#' my.dt <- DataTableWithRItemFormat(my.df, caption = "A nice table")
#' my.dt
#' @importFrom stats df.residual pt pt qt
#' @importFrom DT formatStyle datatable styleInterval styleEqual
#' @importFrom htmltools withTags tags
#' @export
DataTableWithRItemFormat <- function(dd,
                                     caption = NULL,
                                     header.alignments = NULL,
                                     allow.length.change = TRUE,
                                     length.menu = c(10,15,20),
                                     page.length = min(15, nrow(dd)),
                                     allow.paging = TRUE,
                                     show.info = TRUE,
                                     escape.html = TRUE)
{
    if (nrow(dd) == 0 || ncol(dd) == 0)
    {
        warning("Table is too small to be printed as an HTML Data Table.")
        print.default(dd)
        return
    }


    #show.row.names = TRUE
    # Specify the header style information that will be used by datatables to draw the output.
    # For some reason this is handled separately to the style of the cell contents
    header.style <- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #5B9BD5; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white;}"

    caption.style <- "caption-side: bottom; text-align: center; font-family: 'Arial', sans-serif; font-size:10pt; font-weight:normal; color:#505050"

    num.col <- ncol(dd)
    dd$oddoreven <- 1:nrow(dd) %% 2 # Extra dummy column to help us format the table. Will be made invisible later
    col.names <- colnames(dd)
    col.names <- gsub("\\.", " ", col.names)


    # Determine which columns are to be left aligned, and which are to be right-aligned
    if (is.null(header.alignments))
    {
        # Inspect the classes of the columns to determine whether each is to be left or right aligned.
        column.classes <- lapply(dd, class)
        .isNumericClass <- function (c) {
                return(any(c %in% c("numeric", "integer", "logical")) )
        }
        column.is.numeric <- unlist(lapply(column.classes, .isNumericClass))
        right.align.columns <- unname(which(column.is.numeric))
        left.align.columns <- unname(which(!column.is.numeric))
    } else {
        if (length(header.alignments) != num.col)
        {
            stop(paste0("dataTableWithRItemFormat: the number of specified header alignments (",
                        length(header.alignments),
                        ") doesn't match the number of columns in the table (",
                        num.col, ")."))
        }
        if (length(which(header.alignments != "right" & header.alignments != "left")) > 0)
        {
            stop("dataTableWithRItemFormat: header alignments must contain 'left' or 'right' only.")
        }

        right.align.columns <- which(header.alignments == "right")
        left.align.columns <- which(header.alignments == "left")
    }

    column.to.color.by <- num.col + 1
    #if (show.row.names)
    #{

    header.names <- c(" ", col.names)
    column.to.remove <- num.col + 1

    # The container parameter allows us to design the header of the table
    # using CSS
    my.container <-  withTags(table(
        style(type = "text/css", header.style),
        thead(
            tr(
                    lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
            )
        )
    ))

    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = allow.length.change,
                       lengthMenu = length.menu,
                       pageLength = page.length,
                       paging = allow.paging,
                       info = show.info,
                       columnDefs = list(list(targets = column.to.remove, visible = FALSE),
                                         list(targets = " ", className = 'dt-center'),
                                         list(targets = left.align.columns, className = 'dt-left'),
                                         list(targets = right.align.columns, className = 'dt-right'))
                       )

    mydt <- datatable(dd,
                      rownames = TRUE,
                      class = 'hover', # Built-in class with least amount of existing formatting. Have to choose a class.
                      container = my.container,
                      options = my.options,
                      caption = tags$caption(style = caption.style, caption),
                      width = '100%',
                      height = '100%',
                      fillContainer = FALSE,
                      escape = escape.html)


    # Format cells in main table
    mydt <- formatStyle(mydt,
                            columns = 1:num.col,
                            valueColumns = column.to.color.by,
                            fontFamily = "Arial",
                            fontSize = "13px",
                            paddingRight = "1em",
                            borderRightWidth = "1px",
                            borderRightStyle = "solid",
                            borderRightColor = "white",
                            borderBottomColor = "rgb(255, 255, 255)",
                            borderBottomStyle = "solid",
                            borderBottomWidth = "1px",
                            borderCollapse = "collapse",
                            marginBottom = "0px",
                            marginLeft = "0px",
                            marginRight = "0px",
                            marginTop = "0px",
                            paddingBottom = "0px",
                            paddingLeft = "5.2px",
                            paddingRight = "13px",
                            paddingTop = "0px",
                            verticalAlign = "middle",
                            wordWrap = "break-word",
                            backgroundColor = styleEqual(c(1,0), c('rgb(234, 243, 250)', 'rgb(207, 226, 243)'))
    )

    # Row names in blue
    mydt <- formatStyle(mydt,
                            columns = " ",
                            backgroundColor = "rgb(91, 155, 213)",
                            borderBottomColor = "rgb(255, 255, 255)",
                            borderBottomStyle = "solid",
                            borderBottomWidth = "1px",
                            borderCollapse = "collapse",
                            borderRightColor = "rgb(255, 255, 255)",
                            borderRightStyle = "solid",
                            borderRightWidth = "1px",
                            color = "rgb(255, 255, 255)",
                            cursor = "default",
                            emptyCells = "show",
                            fontFamily = "Arial",
                            fontSize = "13px",
                            fontWeight = "bold",
                            lineHeight = "normal",
                            paddingBottom = "2.6px",
                            paddingLeft = "5.2px",
                            paddingRight = "5.2px",
                            paddingTop = "2.6px",
                            textAlign = "left",
                            verticalAlign = "middle"
    )

    return(mydt)
}

# Takes a data table and adds significance higlighting for the coefficients column based on the
# relevant othet statistic in the table.

#' \code{AddSignificanceHighlightingToDataTable}
#'
#' @description Add red and blue highlighting to a data table conditionally on
#'   the values in a specified column. Used to replicate Q's significance
#'   highlighting in the table.
#'
#' @param dt An HTML widget DataTable created e.g. by
#'   \code{\link{DataTableWithRItemFormat}}
#' @param columns.to.color A character vector containing the column names of the
#'   columns that should be colored.
#' @param column.to.check The name of the column whose values will be used to do
#'   the coloring.
#' @param red.value A number specifying the upper bound for values in
#'   \code{column.to.check} which will cause cells in \code{columns.to.color} to
#'   be highlighted red. That is, when cells in column.to.check have a value
#'   less than this, cells in columns.to.color will be colored red.
#' @param blue.value A number specifying the lower bound for coloring cells
#'   blue, as above.
#'
#' @examples
#' my.df <- data.frame(First = c(1,2,3), Second = c("a", "b", "c"))
#' my.dt <- DataTableWithRItemFormat(my.df, caption = "A nice table")
#' my.dt <- AddSignificanceHighlightingToDataTable(my.dt,
#'     columns.to.color = "Second", column.to.check = "First",
#'     red.value = 1.01, blue.value = 2.99)
#'
#' @export
AddSignificanceHighlightingToDataTable <- function(dt, columns.to.color, column.to.check, red.value, blue.value)
{
    new.dt <- formatStyle(dt, columns = columns.to.color,
                              valueColumns = column.to.check,
                              color = styleInterval(c(red.value, blue.value), c('rgb(255,0,0)', 'rgb(0,0,0)', 'rgb(0,0,255)'))
                              )
    return(new.dt)

}



# Create an HTML widget data table (package DT) from the coefficients
# table in a regression summary.
#' @importFrom stats df.residual
#' @importFrom stats pt
#' @importFrom stats qnorm
#' @importFrom stats qt
createRegressionDataTable <- function(x, p.cutoff, caption = NULL, coeff.digits = 2,
                                      p.digits = 2, coefficient.indices = 1:2,
                                      test.index = 3, p.index = 4,
                                      eps.p = 0.001)
{
  # Given a table of coefficients from a summary of a regression
  # figure out which test has been used and which column the test
  # statistics are found in.
  .findTestInCoefficientTable <- function(coefficient.table) {
    col.names <- colnames(coefficient.table)
    t.col <- which(col.names == "t value")
    z.col <- which(col.names == "z value")
    if (length(t.col) == 0 && length(z.col) == 0)
    {
      test.type <- "none"
      test.column <- NULL

    } else if (length(t.col) > 0 && length(z.col) > 0 || length(t.col) > 1 || length(z.col) > 1) {
      stop("Ambiguous statistical testing information in coefficients table.")
    } else if (length(t.col) > 0) {
      test.type <- "t"
      test.column <- t.col
    } else {
      test.type <- "z"
      test.column <- z.col
    }

    return(list(test.type = test.type, test.column = test.column))
  }

  # Create a formatted array of regression coefficient information that can be passed to an HTMLwidget
  # DataTable.
  .formatRegressionCoefficientMatrix <- function (x, coeff.digits = 2,
                                                  p.digits = 2, coefficient.indices = 1:2,
                                                  test.index = 3, p.index = 4,
                                                  eps.p = 0.001)
  {
    d <- dim(x)
    num.cols <- d[2]

    coefficients <- data.matrix(x)


    tidied.coefficients <- array("", dim = d, dimnames = dimnames(coefficients))

    na.values <- is.na(coefficients)

    normal.indices <- c(coefficient.indices, test.index)
    tidied.coefficients[,normal.indices] <- format(round(coefficients[, normal.indices], digits = coeff.digits), digits = coeff.digits)
    tidied.coefficients[,p.index] <- format.pval(coefficients[, p.index], digits = p.digits, eps = eps.p)

    # Print any NA values
    if (any(na.values))
      tidied.coefficients[na.values] <- "NA"

    return(as.data.frame(tidied.coefficients))
  }



  # Make a pretty table with a caption
  coefs <- x$summary$coefficients
  # Ordered Logit tables don't come with a p-value column
  # so calculate the p's from the
  if (x$type == "Ordered Logit")
  {
    ps = 2*pt(-abs(coefs[, test.index]), df = x$summary$df.residual)
    coefs = cbind(coefs, ps)
  }

  pretty.coefs <- .formatRegressionCoefficientMatrix(coefs, coeff.digits,
                                                     p.digits, coefficient.indices,
                                                     test.index, p.index,
                                                     eps.p)
  pretty.coefs <- as.data.frame(pretty.coefs, stringsAsFactors = FALSE)

  caption <- paste0(caption, "Results highlighted when p <= " , p.cutoff)


  dt <- DataTableWithRItemFormat(pretty.coefs,
                                        caption = caption,
                                        header.alignments = rep("right", ncol(pretty.coefs)),
                                        page.length = nrow(pretty.coefs),
                                        allow.paging = FALSE,
                                        show.info = FALSE)


  # Highlight significant coefficients
  test.info <- .findTestInCoefficientTable(coefs)
  if (test.info$test.type == "t")
  {
    t.val <- qt(p.cutoff / 2, df = df.residual(x))
    dt <- AddSignificanceHighlightingToDataTable(dt, columns.to.color = 1,
                                                        column.to.check = "t value",#test.info$test.column,
                                                        red.value = t.val, blue.value = -1L * t.val)
  } else if (test.info$test.type == "z") {
    z.val <- qnorm(p.cutoff / 2)
    dt <- AddSignificanceHighlightingToDataTable(dt, columns.to.color = 1,
                                                        column.to.check = test.info$test.column,
                                                        red.value = z.val, blue.value = -1L * z.val)
  }

  return(dt)
}
