# Format bars used to visually display information such as R-squared.
#' @importFrom formattable formatter percent
createBarFormatter <- function(decimals = 2, bar.shows.magnitude = FALSE, min.display.value = NA,
                               max.display.value = NA, show.as.percent = FALSE,
                               shaded = FALSE, reverse = FALSE)
{
    if (is.na(max.display.value))
    {
        .get.bar.widths <- if (bar.shows.magnitude)
            function(x) percent(abs(x) / max(abs(x), na.rm = TRUE))
        else
            function(x) percent(pmax(x, 0) / max(pmax(x, 0), na.rm = TRUE))
    }
    else
    {
        .get.bar.widths <- if (bar.shows.magnitude)
            function(x) percent(abs(x) / max.display.value)
        else
            function(x) percent(pmax(x, 0) / max.display.value)
    }

    format.fn <- if (show.as.percent) FormatAsPercent else FormatAsReal
    decimals <- decimals # force evaluation of promise before passing to .format.values
    .format.values <- function(x, min.display.value)
    {
        result <- format.fn(x, decimals = decimals)
        result[abs(x) < min.display.value] <- ""
        result
    }

    if (!reverse)
    {
        start.color = positiveSignificanceLighter()
        end.color = positiveSignificanceColour()
    }
    else
    {
        start.color = positiveSignificanceColour()
        end.color = positiveSignificanceLighter()
    }
    formatter(.tag = "span", style = x ~ style(
        display = "inline-block",
        direction = "rtl",
        `border-radius` = "4px",
        `padding-right` = "0px",
        `background-color` = if (shaded) csscolor(gradient(as.numeric(x),
                                                           start.color,
                                                           end.color)) else barColour(),
        width = .get.bar.widths(x)),
        # We need to insert a left-to-right mark so that the minus sign
        # in negative values is not reversed due to the rtl direction.
        x ~ paste0(leftToRightMarkPlaceholder(), .format.values(x, min.display.value)))
}

# We use this placeholder to identify where to insert &lrm;
# into the table html. We cannot insert it directly as format_table
# will escape the ampersand.
leftToRightMarkPlaceholder <- function()
{
    "Replace me with the left-to-right mark"
}

emSpacePlaceholder <- function()
{
    "Replace me with an em space"
}

coloredCirclePlaceholder <- function(circle.color)
{
    paste0(openTagPlaceholder(), "font color='", circle.color,
           "' style='font-size:16px'", closeTagPlaceholder(), circlePlaceholder(),
           openTagPlaceholder(), "/font", closeTagPlaceholder())
}

circlePlaceholder <- function()
{
    "Replace me with a filled circle"
}

openTagPlaceholder <- function()
{
    "Replace me with a less than symbol"
}

closeTagPlaceholder <- function()
{
    "Replace me with a greater than symbol"
}

nonBreakingSpacePlaceholder <- function()
{
    "Replace me with a non-breaking space"
}

lineBreakPlaceholder <- function()
{
    "Replace me with a line break"
}

#' @title Placeholder for html open bold tag <b>
#' @description Insert this into table text in order to add the html open bold
#' tag, since <b> will be escaped if inserted directly.
#' @return Placeholder text for the open bold tag.
#' @export
OpenBoldTagPlaceholder <- function()
{
    "Replace me with an open bold tag"
}

#' @title Placeholder for html close bold tag </b>
#' @description Insert this into table text in order to add the html close bold
#' tag, since </b> will be escaped if inserted directly.
#' @return Placeholder text for the close bold tag.
#' @export
CloseBoldTagPlaceholder <- function()
{
    "Replace me with a close bold tag"
}

#' @importFrom stringr str_locate_all
thickenRowLines <- function(html, row.lines.to.thicken)
{
    thickened.row.tag <- "<tr style='border-top: 2px solid #ddd'>"
    # needs to be same length as thickened.row.tag
    extended.placeholder <- "<tr xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx>"
    html <- gsub("<tr>", extended.placeholder, html)
    # remove first row which is the column header
    positions <- str_locate_all(html, extended.placeholder)[[1]][-1, ]
    for (i in row.lines.to.thicken)
        substr(html, positions[i, 1], positions[i, 2]) <- thickened.row.tag
    html <- gsub(extended.placeholder, "<tr>", html)
    html
}

# Format p-values.
#' @importFrom formattable formatter
createPFormatter <- function(p.cutoff = 0.05)
{
    formatter("span",
    style = p ~ ifelse(p <= p.cutoff, style(font.weight = "bold"), NA),
    p ~ {
            p.formatted <- FormatAsReal(p, decimals = 3)
            p.formatted <- gsub(x = p.formatted, pattern="^(-?)0", replacement="\\1")
            p.formatted[p < 0.001] <- "< .001"
            p.formatted
        }
    )
}

# Colors a statistic cell based on heatmapColourScale, and bolds the text if
# p-value is below the cutoff.
#' @importFrom formattable formatter style
createStatisticFormatter <- function(statistic.name, p.name, p.cutoff = 0.05, max.abs = 5, decimals = 2)
{
    decimals <- decimals # force evaluation of promise before passing to .format.values

    txt <- sprintf("~ style(display = \"block\", padding = \"0 4px\", `border-radius` = \"4px\",
                   `font-weight` = ifelse(%s <= p.cutoff, \"bold\", NA),
                   `background-color` = heatmapColourScale(%s, max.abs))", p.name, statistic.name)
    formatter("span", style = eval(parse(text = txt)), x ~ FormatAsReal(x, decimals = decimals))
}

# Format regression coefficients
#' @importFrom formattable formatter
createEstimateFormatter <- function(statistic.name, p.name, p.cutoff = 0.05, decimals = 2, suffix = "")
{
    txt <- sprintf("~ ifelse(%s <= p.cutoff & %s < 0, \"color:red\",
                   ifelse(%s <= p.cutoff & %s > 0, \"color:blue\", NA))",
                   p.name, statistic.name, p.name, statistic.name)
    formatter("span", style = eval(parse(text = txt)), x ~ paste0(FormatAsReal(x, decimals = decimals), suffix))
}

# Colors a statistic cell
#' @importFrom formattable formatter style
createSingleColourHeatmapFormatter <- function(min.val, max.val, decimals = 2)
{
    txt <- sprintf("x ~ style(display = \"block\", padding = \"0 4px\", `border-radius` = \"4px\",
                   `background-color` = heatmapSingleColourScale(x, min.val, max.val))")
    formatter("span", style = eval(parse(text = txt)), x ~ FormatAsReal(x, decimals = decimals))
}

#' @importFrom htmltools tags
titleFormat <- function(title)
{
    if (title == "")
        NULL
    else
        tags$h3(class = ".h3",
                style = paste0("color:", titleColour(), "; text-align:left; margin-top:0px; margin-bottom:0"),
                title)
}

#' @importFrom htmltools tags
secondaryTitleFormat <- function(secondary.title)
{
    if (secondary.title == "")
        NULL
    else
        tags$h4(class = ".h4",
                 style = paste0("color:", titleColour(), "; text-align:left; margin-top:5px; margin-bottom:0"),
                 secondary.title)
}

#' @importFrom htmltools tags
subTitleFormat <- function(subtitle)
{
    if (subtitle == "")
        NULL
    else
        tags$h5(class = ".h5",
                style = paste0("color:", subtitleColour(), "; text-align:left; margin-top:5px; margin-bottom:0"),
                subtitle)
}

#' @importFrom rmarkdown html_dependency_jquery html_dependency_bootstrap
#' @importFrom formattable format_table as.htmlwidget formattable
#' @importFrom htmltools tags tagList browsable attachDependencies HTML
#' @importFrom htmlwidgets sizingPolicy
createTable <- function(x, col.names, formatters, title, subtitle, footer, no.wrap.column.headers = FALSE,
                        secondary.title = "", col.names.alignment = NULL, row.lines.to.thicken = NULL)
{
    tag.list <- list(titleFormat(title))
    if (nzchar(secondary.title))
        tag.list[[2]] <- secondaryTitleFormat(secondary.title)
    for (s in subtitle)
        tag.list[[length(tag.list) + 1]] <- subTitleFormat(s)

    if (length(footer) > 1)
    {
        p.start.tag <- paste0(openTagPlaceholder(),
                              "p style='margin-bottom:5px;'",
                              closeTagPlaceholder())
        p.end.tag <- paste0(openTagPlaceholder(), "/p", closeTagPlaceholder())
        footer <- paste0(p.start.tag, footer, p.end.tag, collapse = "")
    }
    tag.list[[length(tag.list) + 1]] <- tags$caption(style="caption-side:bottom;font-style:italic; font-size:90%;",
                                                     footer)
    if (is.null(col.names.alignment) && length(col.names) != 0)
        col.names.alignment <- rep("r", length(col.names))

    tbl <- format_table(
        x,
        col.names = col.names,
        formatters = formatters,
        table.attr = paste0(
            'class = "table table-condensed"',
            'style = "margin:0; border-bottom: 2px solid; border-top: 2px solid; font-size:90%;"',
            sep = " "
        ),
        align = col.names.alignment,
        caption = tagList(tag.list)
    )

    browsable(
        attachDependencies(
            tagList(HTML(tbl)),
            list(
                html_dependency_jquery(),
                html_dependency_bootstrap("default")
            )
        )
    )

    # Replace the placeholders
    tbl.html <- HTML(tbl)
    tbl.html <- gsub(leftToRightMarkPlaceholder(), "&lrm;", tbl.html)
    tbl.html <- gsub(emSpacePlaceholder(), "&emsp;", tbl.html)
    tbl.html <- gsub(nonBreakingSpacePlaceholder(), "&#160;" , tbl.html)
    tbl.html <- gsub(openTagPlaceholder(), "<" , tbl.html)
    tbl.html <- gsub(closeTagPlaceholder(), ">" , tbl.html)
    tbl.html <- gsub(circlePlaceholder(), "&#9679;" , tbl.html)
    tbl.html <- gsub(lineBreakPlaceholder(), "<br>" , tbl.html)
    tbl.html <- gsub(OpenBoldTagPlaceholder(), "<b>" , tbl.html)
    tbl.html <- gsub(CloseBoldTagPlaceholder(), "</b>" , tbl.html)

    ## DS-1445 Remove duplicate caption tag
    ## for (el in rev(tag.list))
    ##     tbl <- sub(">", paste0(">", el), tbl)
    tbl.html <- sub("<caption><h3", "<h3", tbl.html)
    tbl.html <- sub("</caption></caption>", "</caption>", tbl.html)

    # Set vertical alignment to middle for table cells
    tbl.html <- gsub('<td style="', '<td style="vertical-align:middle; ', tbl.html)

    if (!is.null(row.lines.to.thicken))
        tbl.html <- thickenRowLines(tbl.html, row.lines.to.thicken)

    if (no.wrap.column.headers)
        tbl.html <- gsub("<th style=\"text-align:right;\">", "<th style=\"text-align:right;white-space:nowrap;\">", tbl.html)

    # this is a really ugly way to return a htmlwidget
    #  I will have to spend some time thinking through this.
    # start by setting up a dummy formattable
    ftw <- as.htmlwidget(formattable(data.frame(dummy.tag = 0)),
                         sizingPolicy = sizingPolicy(browser.padding = 0))
    # and replace the html with our formatted html from above
    ftw$x$html <- tbl.html
    ftw
}
