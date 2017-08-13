#' Converts a proportion to a percent
#'
#' @param x The number(s)
#' @param digits Number of significant digits.
#' @param decimals Number of decimal places to show. If null, this is ignored.
#' @param remove.leading.0 Removes the initial 0 from numbers that are less than 1.
#' @param comma.for.thousands If TRUE, uses a comma when there are thousands.
#' @param pad If a vector is supplied and \link{decimal.places} is specified, adds spaces to the beginning of numbers so they become decimal aligned.
#' @details Multiplies by 100, keeping \code{digits} or more significant digits and
#' putting a % at the end, and commas if in thousands or more.
#' @export
FormatAsPercent <- function(x, digits = 2, decimals = NULL, remove.leading.0 = FALSE, comma.for.thousands = TRUE, pad = FALSE)
{
    x <- FormatAsReal(x * 100, digits, decimals, remove.leading.0, comma.for.thousands, pad = FALSE)
    x.not.na <- x != "NA"
    x[x.not.na] <- paste0(x[x.not.na], "%")
    if (pad)
        x <- padVector(x)
    return(x)
}

#' Formats real numbers nicely.
#'
#' @param x The number(s)
#' @param digits Number of significant digits.
#' @param decimals Number of decimal places to show. If null, this is ignored.
#' @param remove.leading.0 Removes the initial 0 from numbers that are less than 1.
#' @param comma.for.thousands If TRUE, uses a comma when there are thousands.
#' @param pad If a vector is supplied and \link{decimal.places} is specified, adds spaces to the beginning of numbers so they become decimal aligned.
#' @details Multiplies by 100, keeping \code{digits} or more significant digits and
#' , and commas if in thousands or more.
#' Based on \code{\link{formatC}}.
#' @export
FormatAsReal <- function(x, digits = 2, decimals = NULL, remove.leading.0 = FALSE, comma.for.thousands = TRUE, pad = FALSE)
{
    # Vectoring in situations with missing values
    if(any(is.na(x))){
        if (length(x) == 1)
            return("NA")
        x <- sapply(x, FormatAsReal, digits = digits, decimals = decimals, remove.leading.0 = remove.leading.0, comma.for.thousands = comma.for.thousands)
        if (pad)
            x <- padVector(x)
        return(x)
    }
    if (is.null(decimals)) # Formatting number of digits
    {
        result <- formatC(x, digits = digits, format = "fg",
                             big.mark = if (comma.for.thousands) ',' else 0)
        result <- sub("^\\s+", "", result) #trimming whitespace
    }
    else # Formatting number of decimals
        result <- specifyDecimal(x, decimals, comma.for.thousands)
    # Removing 0s
    if (remove.leading.0)
        result <- removeLeading0(result)
    # Padding
    if (pad)
        result <- padVector(result)
    return(result)
}

#' \code{FormatAsPValue}
#' Formats p relatively nicely, ensuring that values greater than 0.05
#' are not rounded to 0.05, and numbers greater than 0 are not rounded to 0,
#' unless is really close to 0.
#'
#' @param p The number(s)
#' @param p.cutoff Ensures that values are not rounded to this value.
#' @param max.decimals When p is smaller than this value, it is returned as < this value (e.g., "< 0.001").

#' @export
FormatAsPValue <- function(p, p.cutoff = 0.05, max.decimals = 12)
{
    if(any(is.na(p))){
        if (length(p) == 1)
            return("NA")
        return(sapply(p, FormatAsPValue, p.cutoff))
    }
    n.digits <- 2
    if (p < 0)
        return("0")
    if (p < 10 ^ -max.decimals)
    {
        return(paste0("< ", formatC(10 ^ -max.decimals, digits = max.decimals, format = "f"), collapse = ""))

    }
    p.formatted <- formatC(p, digits = n.digits, format = "f")
    # Making sure values greater than 0.05 are not shown as 0.05 due to rounding.
    while(as.numeric(p.formatted) == p.cutoff & n.digits < 12)
    {
        n.digits <- n.digits + 1
        p.formatted <- formatC(p, digits = n.digits, format = "f")
    }
    while(n.digits <= 12 && as.numeric(p.formatted) == 0)
    {
        n.digits <- n.digits + 1
        p.formatted <- if (n.digits > 12)
            "< 0.000000000001"
        else
            formatC(p, digits = n.digits, format = "f")
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

#' specifyDecimal
#' @param x The number(s).
#' @param decimals Number of decimal places to show.
#' @param comma.for.thousands If TRUE, uses a comma when there are thousands.
#' @description Taken in part from https://stackoverflow.com/a/12135122/1547926https://stackoverflow.com/a/12135122/1547926
specifyDecimal <- function(x, decimals = 2, comma.for.thousands = TRUE)
{
    x <- trimws(format(round(x, decimals),
                       nsmall = decimals,
                       big.mark = if(comma.for.thousands) "," else ""))
    return(x)
}



#' removeLeading0
#' @param x The number(s).
removeLeading0 <- function(x)
{
    if (substring(x, 1, 1) == "0")
        x <- substring(x, 2)
    return(x)
}

#' padVector
#' @param x The number(s).
#' @importFrom stringr str_pad
padVector <- function(x)
{
    if (length(x) > 1)
        x <- str_pad(x, width = max(nchar(x)))
    return(x)
}
