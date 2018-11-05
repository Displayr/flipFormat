#' HistTable
#'
#' Creates a pretty formattable table showing histograms
#' @param data.values A dataframe, with each column containing the values to create a histogram.
#' @param class.memberships Class memberships for each respondent.
#' @param class.sizes Sizes of each class as proportions.
#' @param title The title of the table.
#' @param subtitle The subtitle of the table.
#' @param footer The footer of the table.
#' @param bin.size Size of the bins used in the histogram. The breakpoints are given by \code{seq(bin.min, bin.max, bin.size)}.
#' @param bin.min Any value in \code{data.values} smaller then this will be truncated to \code{bin.min}.
#' @param bin.max Any value in \code{data.values} larger then this will be truncated to \code{bin.max}.
#' @param hist.width Width of the histogram cell in any valid CSS size unit
#' @param hist.height Height of the histogram cell
#' @param show.tooltips Whether to display tooltips of the bar heights
#' @param color.negative Whether to show negative bars in coral.
#' @param histogram.column.name Name for the histogram column.
#' @param ... Additional columns to add to the table.
#' @importFrom graphics hist
#' @importFrom htmltools as.tags
#' @importFrom htmlwidgets getDependency
#' @importFrom sparkline sparkline
#' @examples
#' dat <- data.frame(A=rpois(500,5), B=rpois(500,50), C=rpois(500,20))
#' print(HistTable(dat, 'Mean Probability'=c(5,50,100)))
#' @export

HistTable <- function(data.values,
                      class.memberships = NULL,
                      class.sizes = NULL,
                      title = "",
                      subtitle = "",
                      footer = "",
                      bin.size = 5,
                      bin.min = 0,
                      bin.max = 100,
                      hist.width = 100,
                      hist.height = 20,
                      show.tooltips = TRUE,
                      color.negative = FALSE,
                      histogram.column.name = "Distribution",
                      ...)
{
    # Input needs to be a data.frame, because we use lapply
    if (!is.data.frame(data.values))
        data.values <- as.data.frame(data.values)

    # Need to add space to names if they are numeric otherwise they won't
    # display for some reason
    if (suppressWarnings(!any(is.na(as.numeric(colnames(data.values))))))
        colnames(data.values) <- paste0(colnames(data.values), " ")

    color.classes <- !is.null(class.memberships)

    if (color.classes)
        class.colors <- classColors(length(class.sizes))

    histString <- function(xx)
    {
        # Points outside the min and max are placed in the left and right bins
        xx[xx >= bin.max] <- bin.max - 0.5 * bin.size
        xx[xx <= bin.min] <- bin.min + 0.5 * bin.size

        breaks <- round(seq(bin.min, bin.max, bin.size), 6)
        counts <- round(hist(xx, plot = F, breaks = breaks,
                             right = FALSE)$counts / length(xx) * 100, 1)

        if (color.classes)
        {
            n.classes <- max(class.memberships)
            n.bins <- length(counts)
            values <- matrix(0, ncol = n.classes, nrow = n.bins)
            for (i in 1:n.bins)
            {
                cm <- class.memberships[xx >= breaks[i] & xx < breaks[i + 1]]
                values[i, as.numeric(names(which.max(table(cm))))] <- counts[i]
            }
            as.character(as.tags(sparkline(values, type = "bar", zeroColor = "lightgray",
                                           width = hist.width, height = hist.height,
                                           stackedBarColor = class.colors,
                                           disableInteraction = !show.tooltips)))
        }
        else if (color.negative)
        {
            breaks <- breaks[-length(breaks)]
            positive.breaks <- breaks >= 0
            positive.counts <- rep(0, length(counts))
            positive.counts[positive.breaks] <- counts[positive.breaks]
            negative.counts <- rep(0, length(counts))
            negative.counts[!positive.breaks] <- counts[!positive.breaks]
            values <- cbind(positive.counts, negative.counts)
            as.character(as.tags(sparkline(values, type = "bar", zeroColor = "lightgray",
                                           width = hist.width, height = hist.height,
                                           stackedBarColor = c(positiveColour(), negativeColour()),
                                           disableInteraction = !show.tooltips)))
        }
        else
        {
            as.character(as.tags(sparkline(counts, type = "bar", zeroColor = "lightgray",
                                           width = hist.width, height = hist.height,
                                           barColor = positiveColour(),
                                           disableInteraction = !show.tooltips)))
        }
    }

    df <- data.frame("temp" = unlist(lapply(data.values, histString)),
                     ..., # extra stats to report
                     stringsAsFactors = FALSE, check.names = FALSE)
    names(df)[1] <- histogram.column.name

    if (color.classes)
    {
        n.classes <- max(class.memberships)
        class.color.text <- character(n.classes)
        for (i in 1:n.classes)
            class.color.text[i] <- paste0(coloredCirclePlaceholder(class.colors[i]),
                                          nonBreakingSpacePlaceholder(),
                                          "Class",
                                          nonBreakingSpacePlaceholder(),
                                          i, nonBreakingSpacePlaceholder(),
                                          "(", FormatAsPercent(class.sizes[i], decimals = 0), ")")
        subtitle <- c(subtitle, paste(class.color.text,
                                      collapse = emSpacePlaceholder()))
    }

    ft <- createTable(df, colnames(df), list(), title, subtitle, footer,
                      col.names.alignment = c("c", rep("r", length(df) - 1)))
    ft$dependencies <- c(ft$dependencies, getDependency("sparkline","sparkline"))
    ft
}

classColors <- function(n.classes)
{
    # Modified from the default plotly palette
    color.palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                       "#8c564b", "#e377c2", "#bcbd22", "#03E7C5", "#7f7f7f")
    class.colors <- color.palette

    # If we run out of colors from the palette,
    # reuse a lighter version of the palette
    new.colors <- color.palette
    while (length(class.colors) < n.classes)
    {
        new.colors <- lightenColors(new.colors)
        class.colors <- c(class.colors, new.colors)
    }
    class.colors[1:n.classes]
}

#' @importFrom colorspace hex2RGB
lightenColors <- function(hex.colors)
{
    rgb(1 - ((1 - hex2RGB(hex.colors)@coords) * 0.6))
}
