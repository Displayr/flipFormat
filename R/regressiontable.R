#'
#' Creates a pretty formattable table.
#' @param coefficient.table A table of regression coefficients, standard errors, z or t statistics, and p-values.
#' @param t \code{TRUE} if t-statistics, and \code{FALSE} if z-statistics.
#' @param footer Text to place in the footer of the table.
#' @param title The title for the table.
#' @param subtitle Subtitle for the table.
#' @references This is based on code written by Kenton Russell.
#' @importFrom rmarkdown html_dependency_jquery html_dependency_bootstrap
#' @importFrom formattable format_table formatter digits style gradient csscolor as.htmlwidget formattable
#' @importFrom htmltools tags tagList browsable attachDependencies HTML
#' @export
RegressionTable <- function(coefficient.table, t, footer, title = "", subtitle = "")
{
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
    test.statistic <- if (t) "t" else "z"
    subtitle.format <- if (subtitle == "") NULL
    else tags$h5(
        class=".h5",
        style="color:green; text-align:left;",
        subtitle)
    title.format <- if (title == "") NULL else tags$h3(
        class=".h3",
        style="color:blue; text-align:center;",
        title)

    tbl <- format_table(
        coef.df,
        col.names = c(
            "Estimate",
            (if(robust.se) "Robust<br/>SE" else "Standard<br/>Error"),
            paste0("<span style='font-style:italic;'>", test.statistic, "</span>"),
            "<span style='font-style:italic;'>p</span>"
        ),
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
        # formatters = list(
        #     Estimate = estimateFormatter,
        #     "Std. Error" = x~digits(x,2),
        #     t = tFormatter,
        #     p = pFormatter
        # )
        formatters = if(robust.se)
                list(
                    Estimate = estimateFormatter,
                    "Robust SE" = x~digits(x,2),
                    t = tFormatter,
                    p = pFormatter
                )
            else
                list(
                    Estimate = estimateFormatter,
                    "Std. Error" = x~digits(x,2),
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
    ftw <- as.htmlwidget(formattable(data.frame()))
    # and replace the html with our formatted html from above
    ftw$x$html <- HTML(tbl)
    ftw

}
