# Format bars used to visually display information such as R-squared
#' @importFrom formattable formatter percent
createBarFormatter <- function(decimals = 2)
{
    formatter(.tag = "span", style = x ~ style(
        display = ifelse(x > 0, "inline-block", NA),
        direction = ifelse(x > 0, "rtl", NA),
        `border-radius` = ifelse(x > 0, "4px", NA),
        `padding-right` = ifelse(x > 0, "0px", NA),
        `background-color` = ifelse(x > 0, barColour(), NA),
        width = percent(pmax(x, 0) / max(pmax(x, 0), na.rm = TRUE))),
        x ~ FormatWithDecimals(x, decimals))
}

# Format p-values.
#' @importFrom formattable formatter
createPFormatter <- function(p.cutoff = 0.05)
{
    formatter("span",
    style = p ~ ifelse(p <= p.cutoff, style(font.weight = "bold"), NA),
    p ~ {
            p.formatted <- FormatWithDecimals(p, 3)
            p.formatted <- gsub(x = p.formatted, pattern="^(-?)0", replacement="\\1")
            p.formatted[p < 0.001] <- "< .001"
            p.formatted
        }
    )
}

# Format regression coefficients
#' @importFrom formattable formatter
createEstimateFormatter <- function(statistic.name, p.name, p.cutoff = 0.05, decimals = 2)
{
    txt <- sprintf("~ ifelse(%s <= p.cutoff & %s < 0, \"color:red\",
                   ifelse(%s <= p.cutoff & %s > 0, \"color:blue\", NA))",
                   p.name, statistic.name, p.name, statistic.name)
    formatter("span", style = eval(parse(text = txt)), x ~ FormatWithDecimals(x, decimals))
}

#' @importFrom formattable formatter style
createHeatmapFormatter <- function(statistic.name, p.name, p.cutoff = 0.05, max.abs = 5, decimals = 2)
{
    txt <- sprintf("~ style(display = \"block\", padding = \"0 4px\", `border-radius` = \"4px\",
                   `font-weight` = ifelse(%s <= p.cutoff, \"bold\", NA),
                   `background-color` = heatmapColourScale(%s, max.abs))", p.name, statistic.name)
    formatter("span", style = eval(parse(text = txt)), x ~ FormatWithDecimals(x, decimals))
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
subTitleFormat <- function(subtitle)
{
    if (subtitle == "")
        NULL
    else tags$h5(class = ".h5",
                 style = paste0("color:", subtitleColour(), "; text-align:left; margin-top:5px; margin-bottom:0"),
                 subtitle)
}

#' @importFrom rmarkdown html_dependency_jquery html_dependency_bootstrap
#' @importFrom formattable format_table as.htmlwidget formattable
#' @importFrom htmltools tags tagList browsable attachDependencies HTML
#' @importFrom htmlwidgets sizingPolicy
createTable <- function(x, col.names, formatters, title, subtitle, footer)
{
    tbl <- format_table(
        x,
        col.names = col.names,
        formatters = formatters,
        table.attr = paste0(
            'class = "table table-condensed"',
            'style = "margin:0; border-bottom: 2px solid; border-top: 2px solid; font-size:90%;"',
            sep = " "
        ),
        align = rep("r", length(col.names) + 1),
        caption = tagList(
            titleFormat(title),
            subTitleFormat(subtitle),
            tags$caption(
                style="caption-side:bottom;font-style:italic; font-size:90%;",
                footer
            )
        )
    )

    browsable(
        attachDependencies(
            tagList(
                HTML(tbl)),
            list(
                html_dependency_jquery(),
                html_dependency_bootstrap("default")
            )
        )
    )

    # this is a really ugly way to return a htmlwidget
    #  I will have to spend some time thinking through this.
    # start by setting up a dummy formattable
    ftw <- as.htmlwidget(formattable(data.frame()), sizingPolicy = sizingPolicy(browser.padding = 0))
    # and replace the html with our formatted html from above
    ftw$x$html <- HTML(tbl)
    ftw
}
