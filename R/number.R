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
    if(any(is.na(x))){
        if (length(x) == 1)
            return("NA")
        return(sapply(x, FormatAsPercent))
    }
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
    if(any(is.na(x))){
        if (length(x) == 1)
            return("NA")
        return(sapply(x, FormatAsReal))
    }
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
    if(any(is.na(p))){
        if (length(p) == 1)
            return("NA")
        return(sapply(p, FormatAsPValue, p.cutoff))
    }
    n.digits <- 2
    if (p < 0)
        return("0")
    p.formatted <- formatC(p, digits = n.digits, format = "f")
    # Making sure values greater than 0.05 are not shown as 0.05 due to rounding.
    while(as.numeric(p.formatted) == p.cutoff & n.digits < 12)
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

#' Formats real numbers with a specified number of decimal places.
#' @param x The number(s).
#' @param decimal.places Number of decimal places to show.
#' @export
FormatWithDecimals <- function(x, decimal.places = 2)
{
    trimws(format(c(round(x, decimal.places), 0.123456789),
                  digits = decimal.places,
                  scientific = FALSE,
                  big.mark = ",")[1:length(x)])
}
