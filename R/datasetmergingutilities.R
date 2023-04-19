#' @param variable.index Integer scalar of the variable index to be shown.
#' @param num.span.width Integer scalar of the width of the span in pixels.
#' @return Character scalar containing a HTML span showing the variable index
#'  with a predefined width.
#' @noRd
variableIndexSpan <- function(variable.index, num.span.width)
{
    paste0("<span class=\"data-set-widget-var-num\" style=\"width:",
           num.span.width, "px\">", variable.index, ".</span>")
}

#' @param indicator.states Logical vector representing indicator states.
#' @return Character scalar containing the HTML span which shows data set
#'  indicators: filled and unfilled squares indicating whether a variable
#'  contains input from an input data set.
#' @noRd
dataSetIndicatorsSpan <- function(indicator.states)
{
    indicators <- vapply(seq_along(indicator.states), function(j) {
        if (indicator.states[j])
            paste0("<span class=\"data-set-merging-indicator data-set-merging-indicator-fill\" title=\"Data set ",
                   j, "\">&#8193;</span>")
        else
            paste0("<span class=\"data-set-merging-indicator\" title=\"Data set ",
                   j, "\">&#8193;</span>")
    }, character(1))

    paste0("<span class=\"data-set-merging-indicator-container\">",
           paste0(indicators, collapse = ""), "</span>")
}

#' @param var.name Character scalar of the variable name to display.
#' @param var.label Character scalar of the variable label to display.
#' @return Character scalar showing the variable name and label separated by a
#'  colon. If the name and label are the same, only show one of them.
#' @noRd
variableNameAndLabelText <- function(var.name, var.label)
{
    if (nchar(var.label) > 0)
    {
        if (substr(var.label, 1, nchar(var.name) + 1) == paste0(var.name, ":") ||
            var.name == var.label)
            htmlText(var.label)
        else
            paste0(htmlText(var.name), ": ", htmlText(var.label))
    }
    else
        htmlText(var.name)
}

variableIndicesInPage <- function(n.vars, page, variables.per.page)
{
    if (is.null(page)) {
        seq_len(min(n.vars, variables.per.page))
    } else {
        n.pages <- numberOfPages(n.vars, variables.per.page)
        if (page > n.pages) {
            warning("The specified page number exceeds the number of pages. ",
                    "The last page has been shown.")
            page <- n.pages
        }
        start.ind <- (page - 1) * variables.per.page + 1
        end.ind <- min(n.vars, page * variables.per.page)
        start.ind:end.ind
    }
}

numberOfPages <- function(n.vars, variables.per.page)
{
    ceiling(n.vars / variables.per.page)
}

variableIndexSpanWidth <- function(page.var.ind)
{
    ceiling(log10(max(page.var.ind) + 1)) * 10 + 15
}

#' @importFrom utils tail
pageSubtitle <- function(n.vars, page, variables.per.page, page.var.ind)
{
    n.pages <- numberOfPages(n.vars, variables.per.page)
    paste0("Page ", min(page, n.pages), " of ", n.pages,
           " (variables ", head(page.var.ind, 1), " to ",
           tail(page.var.ind, 1), ")")
}
