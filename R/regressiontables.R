#' #devtools::install_github("renkun-ken/formattable@v0.2")
#'
#' library(formattable)
#' library(htmltools)
#'
#' #' @importFrom rmarkdown html_dependency_jquery html_dependency_bootstrap
#' browsable(
#'   attachDependencies(
#'     tagList(HTML(tbl)),
#'     list(
#'       html_dependency_jquery(),
#'       html_dependency_bootstrap("default")
#'     )
#'   )
#' )
#'
#'
#' fixedDigits <- function(x, n = 2) {
#'   formatC(x, digits = n, format = "f")
#' }
#'
#' pFormatter <- formatter(
#'   "span",
#'   style = p ~ ifelse(p <= 0.05, style(font.weight = "bold"), NA),
#'   p ~ {
#'     p.formatted <- fixedDigits(p, 3)
#'     p.formatted <- gsub(x=p.formatted, pattern="^(-?)0", replacement="\\1")
#'     p.formatted[p < 0.001] <- "< .001"
#'     p.formatted
#'   }
#' )
#'
#' estimateFormatter <- formatter(
#'   "span",
#'   style = ~ ifelse(
#'     p <= 0.05 & t < 0,
#'     "color:red",
#'     ifelse(p <= 0.05 & t > 0, "color:blue", NA)
#'   ),
#'   Estimate ~ fixedDigits(Estimate, 2)
#' )
#'
#' # Add tiles to t- and z- statistics.
#' tFormatter <- formatter(
#'   "span",
#'   style = x ~ style(
#'     display = "block",
#'     padding = "0 4px", `border-radius` = "4px",
#'     `background-color` = csscolor(gradient(abs(x),"white", "orange"))
#'   ),
#'   ~ fixedDigits(t, 2)
#' )
#'
#' #'
#' #' @param coefficient.table A table of regression coefficients, standard errors, z or t statistics, and p-values.
#' #' @param t \code{TRUE} if t-statistics, and \code{FALSE} if z-statistics.
#' #' @param footer Text to place in the footer of the table.
#' #'
#' PrettyRegressionTable <- function(coefficient.table, t, footer, title = "", subtitle = "")
#' {
#'
#'     coef.df <- data.frame(coefficient.table, check.names=FALSE)
#'     colnames(coef.df)[3:4] <- c("t","p")
#'     test.statistic <- if (t) "t" else "z"
#'     subtitle.format <- if (subtitle == "") NULL
#'         else tags$h4(
#'             class=".h4",
#'             style="color:green; text-align:left;line-height:0.75;",
#'             subtitle)
#'     title.format <- if (title == "") NULL else tags$h2(
#'           class=".h2",
#'           style="color:blue; text-align:center;line-height:0.75;",
#'           title)
#'
#'     tbl <- format_table(
#'         coef.df,
#'         col.names = c(
#'         "Estimate",
#'         "Standard<br/>Error",
#'         paste0("<span style='font-style:italic;'>", test.statistic, "</span>"),
#'         "<span style='font-style:italic;'>p</span>"
#'       ),
#'       table.attr = paste0(
#'         'class = "table table-condensed"',
#'         'style = "margin:0px 0px 0px 0px; border-bottom: 2px solid; border-top: 2px solid;"',
#'         sep = " "
#'       ),
#'       align = rep("r",5),
#'       caption = tagList(
#'         title.format,
#'         subtitle.format,
#'         tags$caption(
#'           style="caption-side:bottom;font-style:italic;font-size:80%;",
#'           footer
#'         )
#'       ),
#'       formatters = list(
#'         Estimate = estimateFormatter,
#'         "Std. Error" = x~digits(x,2),
#'         t = tFormatter,
#'         p = pFormatter
#'       )
#'     )
#'
#'
#'
#'     # this is a really ugly way to return a htmlwidget
#'     #  I will have to spend some time thinking through this.
#'     # start by setting up a dummy formattable
#'     ftw <- as.htmlwidget(formattable(data.frame()), width="80%")
#'     # and replace the html with our formatted html from above
#'     ftw$x$html <- HTML(tbl)
#'     ftw
#'
#' }
#'
#'
#' ft <- "Yo! This footer specifically designed
#'       to communicate important information.
#'       Since it is so important, it will of course
#'       extend over many lines.  In fact, on narrow tables,
#'       it might take >3.  On wide tables, it might only
#'       require one.  Feel free to adjust the width,
#'       and the importance and significance does not
#'       go away."
#'
#' #
#' #
#' # coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
#' #
#' # .46*rnorm(1000, .36, 0.002) + .54*rnorm(1000,1.28, .14)
#' #
#' # PrettyRegressionTable(coef.matrix, TRUE, ft, title = "My awesome regression", subtitle = "Big brown dog")
