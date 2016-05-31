#' Converts a proportion to a percent
#'
#' @param x The number(s)
#' @param digits Number of significant digits.
#' @details Multiplies by 100, keeping \code{digits} or more significant digits and
#' putting a % at the end, and commas if in thousands or more.
#' Based on \code{\link{formatC}}.

#' @export
FormatAsPercent <- function(x, digits = 2)
{
    result <- paste0(formatC(100 * x, digits = digits, format = "fg",  big.mark=','), "%")
    sub("^\\s+", "", result) #trimming whitespace
}

#' Formats real numbers nicely.
#'
#' @param x The number(s)
#' @param digits Number of significant digits
#' @details Keeping \code{digits} or more significant digits and and commas if in thousands or more.
#' @export
FormatAsReal <- function(x, digits = 2)
{
    result <- formatC(x, digits = digits, format = "fg", big.mark=',')
    sub("^\\s+", "", result) #trimming whitespace
}
#' \code{FormatAsPValue}
#' Formats p relatively nicely, ensuring that values greater than 0.05
#' are not rounded to 0.05, and numbers greater than 0 are not rounded to 0,
#' unless is really close to 0.
#'
#' @param p The number(s)
#' @param p.cutoff TODO

#' @export
FormatAsPValue <- function(p, p.cutoff = 0.05)
{
    n.digits <- 2
    if (p < 0)
        return("0")
    p.formatted <- formatC(p, digits = n.digits, format = "f")
    # Making sure values greater than 0.05 are not shown as 0.05 due to rounding.
    while(as.numeric(p.formatted) == p.cutoff)
    {
        n.digits <- n.digits + 1
        p.formatted <- formatC(p, digits = n.digits, format = "f")
    }
    # Making sure values greater than 0.05 are not shown as 0.05 due to rounding.
    while(as.numeric(p.formatted) == 0 & n.digits < 12)
    {
        n.digits <- n.digits + 1
        p.formatted <- formatC(p, digits = n.digits, format = "f")
    }
    p.formatted
}
