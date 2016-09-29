#' MeanComparisonsTable
#'
#' Creates a pretty formattable table.
#' @param means The means to be shown on the table.
#' @param zs Z-Statistics. Only used to determine the color of the fonts.
#' @param ps P-Values associated with each mean. Used to determine whether or not to color the cells.
#' @param r.squared The r-squared value for the regression predicting the row variable by the columns.
#' @param overall.p The P-value for the regression (e.g., an ANOVA F-Test for a linear model).
#' @param column.names The names to put on the columns of the table (other than for the R-Squared and overall P).
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param p.cutoff The alpha level used when formatting the p-value column.
#' @references This is based on code written by Kenton Russell.
#' @importFrom rmarkdown html_dependency_jquery html_dependency_bootstrap
#' @importFrom formattable format_table formatter digits style gradient csscolor as.htmlwidget formattable color_tile percent
#' @importFrom htmltools tags tagList browsable attachDependencies HTML
#' @importFrom htmlwidgets sizingPolicy
#' @export
MeanComparisonsTable <- function(means, zs, ps, r.squared, overall.p, column.names, footer, title = "", subtitle = "", p.cutoff = 0.05)
{
    # Putting all the tables into a single data.frame, as required by formattable.
    ps[zs < 0] <- -ps[zs < 0]
    winsorized.zs <- zs
    winsorized.zs[zs < -5] <- -5
    winsorized.zs[zs > 5] <- 5
    colnames(winsorized.zs) <- paste0(LETTERS[1:ncol(zs)], 2)
    means <- as.data.frame(cbind(means, ps, rsquared = r.squared, pvalue = overall.p, winsorized.zs))
    column.names <- c(column.names, "R-Squared", "<i>p</i>")
    k <- length(column.names) #Number of being compared.

    # Set the number of decimals
    fixedDigits <- function(x, n = 2) {
        formatC(x, digits = n, format = "f")
    }
    # FOrmat the p-values.
    pFormatter <- formatter(
        "span",
        style = p ~ ifelse(p <= p.cutoff, style(font.weight = "bold"), NA),
        p ~ {
            p.formatted <- fixedDigits(p, 3)
            p.formatted <- gsub(x = p.formatted, pattern="^(-?)0", replacement="\\1")
            p.formatted[p < 0.001] <- "< .001"
            p.formatted
        }
    )

    .colorScale <- function(x, min = -5, max = 5)
    {
        csscolor(gradient(c(min, max, x), "white", "#00C8C8"))[-2:-1]
    }
    rsquaredFormatter <- formatter(.tag = "span", style = function(x) style(
        display = "inline-block", direction = "rtl", `border-radius` = "4px", `padding-right` = "0px",
        `background-color` = "pink", width = percent(x / max(x))), ~ fixedDigits(rsquared, 2))

    formatters <- list()
    for (i in 1:k)
    {
        l <- LETTERS[i]
        txt <- sprintf("~ style(color = ifelse(abs(%s1) <= 0.05 & %s1 < 0, \"red\",ifelse(abs(%s1) <= 0.05, \"blue\", NA)),
                       display = \"block\", padding = \"0 4px\", `border-radius` = \"4px\",
                       `font-weight` = ifelse(abs(%s1) <= 0.05, \"bold\", NA),
                       `background-color` = .colorScale(%s2))", l, l, l, l, l)
        formatters[[l]] <- formatter("span", style = eval(parse(text = txt)),
                                     eval(parse(text = sprintf("%s~ fixedDigits(%s, 2)", l, l))))
    }
    formatters[["rsquared"]] <- rsquaredFormatter
    formatters[["pvalue"]] <- pFormatter

    # Removing unwanted variables (i.e., the variables that contain the p-values and z statistics)
    p.values <- rep(FALSE, k)
    names(p.values) <- paste0(LETTERS[1:k], "1")
    z.stats <- rep(FALSE, k)
    names(z.stats) <- paste0(LETTERS[1:k], "2")
    formatters <- c(formatters, as.list(p.values), as.list(z.stats))
    subtitle.format <- if (subtitle == "") NULL else tags$h5(class=".h5",
        style=paste0("color:", subtitleColour(), "; text-align:left; margin-top:5px; margin-bottom:0"), subtitle)
    title.format <- if (title == "") NULL else tags$h3(class=".h3",
        style=paste0("color:", titleColour(), "; text-align:left; margin-top:0px; margin-bottom:0"),title)
    tbl <- format_table(means,
                    col.names = column.names,
                    table.attr = paste('class = "table table-condensed"',
                                        'style = "margin:0; border-bottom: 2px solid; border-top: 2px solid; font-size:90%;"'),
                    align = rep("r",5),
                    caption = tagList(title.format,
                                      subtitle.format,
                                      tags$caption(style="caption-side:bottom;font-style:italic;font-size:90%;",
                                    footer)),
                    formatters = formatters)
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
