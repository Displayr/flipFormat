#' \code{"Label<-"}
#'
#' Assign a "label" attribute to an object.
#' @param x The object.
#' @param value A \code{character} object to set as a value.
#' @export
`Label<-` <- function(x, value) {
  attr(x, "label") <- value
  x
}


#' \code{Label}
#'
#' Extracts the "label" attribute from an object. Returns the name if the label does not exist.
#' @param x The object.
#' @export
Label <- function(x)
{
    result <- attr(x, "label")
    if(is.null(result))
        result <- deparse(substitute(x))
    return(result)
}


#' \code{Labels}
#'
#' Replaces a list of given names or coefficient, with any underlying labels.
#' @param data A \code{\link{data.frame}}.
#' @param names An optional list of The names of the variables or coefficients (e.g., Q2Cola for a factor).
#' @return A \code{vector} of labels
#' @details Returns the names if the labels to not exist.
#' Where \code{names} is provided, Works for dummy variables as well as normal variables.
#' Trims backticks and whitespace. Returns names where labels cannot be found.
#' @export
Labels <- function(data, names = NULL)
{
    if (is.null(names))
    {
        result <- sapply(data, Label)
        names(result) <- names(data)
        return(result)
    }
    #####  Creating a list of all the possible variable and coefficient names that can have labels.
    # The labels
    labels.list <- lapply(data, function(x) attr(x, "label"))
    # Removig the names of elements in the list, because  unlist changes
    # "name" to "name.name" and "`name`" to "'name'.name".
    for (i in seq_along(labels.list))
    {
        if (!is.null(labels.list[[i]]))
            names(labels.list[[i]]) <- NULL
    }
    possible.labels <- unlist(labels.list)
    possible.names <- names(possible.labels)
   # # Fixing up the names, which hav been written as name.name by unlist.
   #  lengths <- sapply(possible.names, function(x) floor(nchar(x)))
   #  true.lengths <- sapply(possible.names, function(x) floor(nchar(x)/2))
   #  has.backtick <- substring(possible.names, 1, 1) == "`"
   #  true.lengths <- ifelse(has.backtick, true.lengths + 1, true.lengths)
   #  possible.names <- substring(possible.names, 1, true.lengths)
   #  # Factor coefficient names (e.g., Q2Male, Q2Female)).
    data.possible <- data[, possible.names]
    factors <- sapply(data.possible, is.factor)
    possible.factor.levels <- lapply(data.possible, levels)
    possible.factor.levels <- Filter(Negate(function(x) is.null(unlist(x))), possible.factor.levels) # Removing NULLs
    possible.factor.names <- possible.names[factors]
    possible.factor.labels <- possible.labels[factors]
    name.factors <- NULL
    label.factors <- NULL
    for (i in seq_along(possible.factor.names))
    {
        nm <- possible.factor.names[i]
        label <- possible.factor.labels[i]
        levs <- possible.factor.levels[[i]]
        # Backticks sometimes appearing around variable names.
        name.factors <- c(name.factors, c(paste0(nm, levs), paste0("`", nm, "`",  levs)))
        label.factors <- c(label.factors, rep(paste0(label, ": ", levs), 2))
    }
    # Putting it all together, including dealing with variables  with back ticks.
    possible.labels <- c(possible.labels, possible.labels, label.factors)
    possible.names <- c(possible.names, paste0("`", possible.names, "`"), name.factors)
    # Substituting.
    matches <- match(names, possible.names)
    names[!is.na(matches)] <- possible.labels[matches[!is.na(matches)]]
    names <- removeBackTicks(names)
    names <- TrimWhitespace(names)
    names
}

# Removes any backticks surrouding a variable, if they are there.
removeBackTicks <- function(x)
{
    have.backticks <- as.integer(substr(x, 1, 1) == "`")
    lengths <- nchar(x)
    substr(x, 1 + have.backticks, nchar(x) - have.backticks)
}

#' \code{VariableLabels}
#' Replaces a list of given names, with any underlying labels, if they exist in the data.
#' @param data A \code{\link{data.frame}}.
#' @return A \code{vector} of labels or names.
#' @export
VariableLabels <- function(data)
{
    list.of.labels <- sapply(data, function(x) attr(x, "label"))
    variable.names <- names(data)
    missing <- is.null(list.of.labels)
    list.of.labels[missing] <- variable.names[missing]
    list.of.labels
}
