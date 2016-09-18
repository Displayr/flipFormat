#'
#' Creates a pretty formattable table.
#' @param coefficient.table A table of regression coefficients, standard errors, z or t statistics, and p-values.
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @param estimate.name The name of the Estimate column. Defaults to "Estimate".
#' @param se.name The name of the standard error column. Defaults to "Standard<br/>Error".
#' @param statistic.name The name of the test-statistic column. Defaults to "<span style='font-style:italic;'>t</span>"".
#' @param p.name The name of the p-value column. Defalts to "Estimate".
#' @references This is based on code written by Kenton Russell.
#' @importFrom rmarkdown html_dependency_jquery html_dependency_bootstrap
#' @importFrom formattable format_table formatter digits style gradient csscolor as.htmlwidget formattable
#' @importFrom htmltools tags tagList browsable attachDependencies HTML
#' @importFrom htmlwidgets sizingPolicy
#' @export
RegressionTable <- function(coefficient.table,
                            footer,
                            title = "",
                            subtitle = "",
                            estimate.name = "Estimate",
                            se.name = "Standard<br/>Error",
                            statistic.name = "<span style='font-style:italic;'>t</span>",
                            p.name = "<span style='font-style:italic;'>p</span>")
{
    col.names <- c(estimate.name, se.name, statistic.name, p.name)
    # Standardizing column names to simplify formattable calls
    colnames(coefficient.table) <- c("Estimate", "SE", "p", "t")
    robust.se <- colnames(coefficient.table)[2] == "Robust SE"
    # Set the number of decimails
    fixedDigits <- function(x, n = 2) {
        formatC(x, digits = n, format = "f")
    }
    # FOrmat the p-values.
    pFormatter <- formatter(
        "span",
        style = p ~ ifelse(p <= 0.05, style(font.weight = "bold"), NA),
        p ~ {
            p.formatted <- fixedDigits(p, 3)
            p.formatted <- gsub(x = p.formatted, pattern="^(-?)0", replacement="\\1")
            p.formatted[p < 0.001] <- "< .001"
            p.formatted
        }
    )

    # Add tiles to t- and z- statistics.
    .colorScale <- function(x)
    { # Creates a color range where 0 is white
        abs.x <- abs(x)
        abs.x[abs.x < 1.959964] <- 0
        abs.x[is.na(x)] <- 0
        min.x <- min(abs.x)
        lower <- if (min.x == 0) "white" else
             gradient(c(0, min.x, abs.x),"white", "orange")[2]
        csscolor(gradient(abs.x, lower, "orange"))
    }
    tFormatter <- formatter(
        "span",
        style = x ~ style(
            display = "block",
            padding = "0 4px", `border-radius` = "4px",
            `background-color` = .colorScale(x)
        ),
        ~ fixedDigits(t, 2)
    )
    estimateFormatter <- formatter(
        "span",
        style = ~ ifelse(
            p <= 0.05 & t < 0,
            "color:red",
            ifelse(p <= 0.05 & t > 0, "color:blue", NA)
        ),
        Estimate ~ fixedDigits(Estimate, 2)
    )

    coef.df <- data.frame(coefficient.table, check.names=FALSE)
    colnames(coef.df)[3:4] <- c("t","p")
    subtitle.format <- if (subtitle == "") NULL
    else tags$h5(
        class=".h5",
        style=paste0("color:", subtitleColour(), "; text-align:left; margin-top:2px; margin-bottom:0"),
        subtitle)
    title.format <- if (title == "") NULL else tags$h3(
        class=".h3",
        style=paste0("color:", titleColour(), "; text-align:left; margin-top:0px; margin-bottom:0"),
        title)

    tbl <- format_table(
        coef.df,
        col.names = col.names,
        table.attr = paste0(
            'class = "table table-condensed"',
            'style = "margin:0; border-bottom: 2px solid; border-top: 2px solid; font-size:90%;"',
            sep = " "
        ),
        align = rep("r",5),
        caption = tagList(
            title.format,
            subtitle.format,
            tags$caption(
                style="caption-side:bottom;font-style:italic; font-size:90%;",
                footer
            )
        ),
        formatters = list(
                    Estimate = estimateFormatter,
                    SE = x ~ digits(x, 2),
                    t = tFormatter,
                    p = pFormatter
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

titleColour <- function()
{
    "#3E7DCC"
}

subtitleColour <- function()
{
    "#A6A6A6"
}
