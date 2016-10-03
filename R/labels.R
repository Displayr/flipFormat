#' \code{"Label<-"}
#'
#' Assign a "label" attribute to an object.
#' @param x An object or a list.
#' @param value A \code{character} object to set as a value or a vector of values.
#' @export
`Labels<-` <- function(x, value) {
    if (is.list(x))
        for (i in seq_along(x))
            attr(x[[i]], "label") <- value[i]
    else
        attr(x, "label") <- value
  x
}



#' \code{Labels}
#'
#' Replaces a list of given names or coefficient, with any underlying labels. If
#' @param x A \code{\link{data.frame}}.
#' @param names.to.lookup An optional list of The names of the variables or coefficients (e.g., Q2Cola for a factor).
#' @param show.name If \code{TRUE}, the name will prefix the extended label (where they are distinct). Ignored if
#' \code{names.to.lookup} is provided.
#' @return A \code{vector} of labels
#' @details First tries to find the "label" attribute, then "name", then "question", and lastly looks to the variable's name.
#' Where \code{names.to.lookup} is provided, Works for dummy variables as well as normal variables.
#' Trims backticks and whitespace. Returns names where labels cannot be found.
#' @export
Labels <- function(x, names.to.lookup = NULL, show.name = FALSE)
{
    .createLabel <- function(name, label, question, x, show.name)
    {
        if (is.list(name))
        {
            for (i in seq_along(name))
                name[[i]] <- .createLabel(name[[i]], label[[i]], question[[i]], x[[i]], show.name)
            return(name)
        }
        if (is.null(label) | show.name)
        {
            if (is.null(name))
                name <- OriginalName(x)
        }
        if (is.null(label))
            label <- question
        else
        {
            if (!is.null(question) && question != label)
                label <- paste0(question, ": ", label)
        }
        if (!show.name & !is.null(label))
            return(label)
        if (is.null(name))
            name <- OriginalName(x)
        if (!show.name | is.null(label))
            return(name)
        paste0(label, " (", name, ")")
    }
    # Single variable case.
    if(!is.list(x))
    {
        name <- attr(x, "name")
        label <- attr(x, "label")
        question <- attr(x, "question")
        return(.createLabel(name, label, question, x, show.name))
    }
    # Data frame case.
    if (is.null(names.to.lookup))
    {
        nms <- names(x)
        for (i in 1:length(x))
        {
            if (is.null(attr(x[[i]], "name")))
                attr(x[[i]], "name") <- nms[i]
        }
        result <- sapply(x, function(s) Labels(s, show.name = show.name))
        names(result) <- nms
        no.label <- result == "X[[i]]" | is.na(result)
        result[no.label] <- names(x)[no.label]
        return(result)
    }
    #####  Creating a list of all the possible variable and coefficient names that can have labels.
    # The labels
    labels.list <- lapply(x, function(x) attr(x, "label"))
    possible.names <- names(labels.list)
    questions.list <- lapply(x, function(x) attr(x, "question"))
    name.list <- as.list(names(x))
    labels.list <- .createLabel(name.list,labels.list,  questions.list, x, show.name)
    # Removig the names of elements in the list, because  unlist changes
    # "name" to "name.name" and "`name`" to "'name'.name".
    for (i in seq_along(labels.list))
    {
        if (!is.null(labels.list[[i]]))
            names(labels.list[[i]]) <- NULL
    }
    possible.labels <- unlist(labels.list)
   # # Fixing up the names, which hav been written as name.name by unlist.
    x.possible <- x[, possible.names]
    factors <- sapply(x.possible, is.factor)
    possible.factor.levels <- lapply(x.possible, levels)
    possible.factor.levels <- Filter(Negate(function(x) is.null(unlist(x))), possible.factor.levels) # Removing NULLs
    possible.factor.names <- possible.names[factors]
    possible.factor.labels <- possible.labels[factors]
    name.factors <- NULL
    label.factors <- NULL
    for (i in seq_along(possible.factor.names))
    {
        nm <- possible.factor.names[i]
        label <- possible.factor.labels[i]
        levs <- possible.factor.levels[[nm]]
        # Backticks sometimes appearing around variable names.
        name.factors <- c(name.factors, c(paste0(nm, levs), paste0("`", nm, "`",  levs)))
        label.factors <- c(label.factors, rep(paste0(label, ": ", levs), 2))
    }
    # Putting it all together, including dealing with variables  with back ticks.
    possible.labels <- c(possible.labels, possible.labels, label.factors)
    possible.names <- c(possible.names, paste0("`", possible.names, "`"), name.factors)
    # Substituting.
    matches <- match(names.to.lookup, possible.names)
    names.to.lookup[!is.na(matches)] <- possible.labels[matches[!is.na(matches)]]
    names.to.lookup <- removeBackTicks(names.to.lookup)
    names.to.lookup <- TrimWhitespace(names.to.lookup)
    names.to.lookup
}

# Removes any backticks surrouding a variable, if they are there.
removeBackTicks <- function(x)
{
    have.backticks <- as.integer(substr(x, 1, 1) == "`")
    lengths <- nchar(x)
    substr(x, 1 + have.backticks, nchar(x) - have.backticks)
}
