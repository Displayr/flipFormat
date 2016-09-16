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
#' @references This is based on code written by Kenton Russell.
#' @importFrom rmarkdown html_dependency_jquery html_dependency_bootstrap
#' @importFrom formattable format_table formatter digits style gradient csscolor as.htmlwidget formattable color_tile
#' @importFrom htmltools tags tagList browsable attachDependencies HTML
#' @importFrom htmlwidgets sizingPolicy
#' @export
MeanComparisonsTable <- function(means, zs, ps, r.squared, overall.p, column.names, footer, title = "", subtitle = "")
{
    # Putting all the tables into a single data.frame, as required by formattable.
    ps[zs < 0] <- -ps[zs < 0]
    means <- as.data.frame(cbind(means, ps, rsquared = r.squared, pvalue = overall.p))
    column.names <- c(column.names, "R-Squared", "<i>p</i>")


    k <- length(column.names) #Number of being compared.

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

    # Add tiles to R-squarest- and z- statistics.
    .colorScale <- function(x)
    { # Creates a color range where 0 is white
        min.x <- min(x)
        max.x <- max(x)
        #lower <- gradient(c(0, min.x, max.x),"white", "orange")[2]
        csscolor(gradient(x, "white", "orange"))
    }
    rsquaredFormatter <-formatter(
        "span",
        style = x ~ style(
            display = "block",
            padding = "0 4px", `border-radius` = "4px",
            `background-color` = .colorScale(x)
        ),
        ~ fixedDigits(rsquared, 2)
    )

    color_tile("white", "orange")

    formatters <- list(A = formatter("span",style = ~ ifelse(abs(A1) <= 0.05 & A1 < 0, "color:red",ifelse(abs(A1) <= 0.05, "color:blue", NA)), A~ fixedDigits(A, 2)),
                       B = formatter("span",style = ~ ifelse(abs(B1) <= 0.05 & B1 < 0, "color:red",ifelse(abs(B1) <= 0.05, "color:blue", NA)), B~ fixedDigits(B, 2)),
                       C = formatter("span",style = ~ ifelse(abs(C1) <= 0.05 & C1 < 0, "color:red",ifelse(abs(C1) <= 0.05, "color:blue", NA)), C~ fixedDigits(C, 2)),
                       D = formatter("span",style = ~ ifelse(abs(D1) <= 0.05 & D1 < 0, "color:red",ifelse(abs(D1) <= 0.05, "color:blue", NA)), D~ fixedDigits(D, 2)),
                       E = formatter("span",style = ~ ifelse(abs(E1) <= 0.05 & E1 < 0, "color:red",ifelse(abs(E1) <= 0.05, "color:blue", NA)), E~ fixedDigits(E, 2)),
                       F = formatter("span",style = ~ ifelse(abs(F1) <= 0.05 & F1 < 0, "color:red",ifelse(abs(F1) <= 0.05, "color:blue", NA)), F~ fixedDigits(F, 2)),
                       G = formatter("span",style = ~ ifelse(abs(G1) <= 0.05 & G1 < 0, "color:red",ifelse(abs(G1) <= 0.05, "color:blue", NA)), G~ fixedDigits(G, 2)),
                       H = formatter("span",style = ~ ifelse(abs(H1) <= 0.05 & H1 < 0, "color:red",ifelse(abs(H1) <= 0.05, "color:blue", NA)), H~ fixedDigits(H, 2)),
                       I = formatter("span",style = ~ ifelse(abs(I1) <= 0.05 & I1 < 0, "color:red",ifelse(abs(I1) <= 0.05, "color:blue", NA)), I~ fixedDigits(I, 2)),
                       J = formatter("span",style = ~ ifelse(abs(J1) <= 0.05 & J1 < 0, "color:red",ifelse(abs(J1) <= 0.05, "color:blue", NA)), J~ fixedDigits(J, 2)),
                       K = formatter("span",style = ~ ifelse(abs(K1) <= 0.05 & K1 < 0, "color:red",ifelse(abs(K1) <= 0.05, "color:blue", NA)), K~ fixedDigits(K, 2)),
                       L = formatter("span",style = ~ ifelse(abs(L1) <= 0.05 & L1 < 0, "color:red",ifelse(abs(L1) <= 0.05, "color:blue", NA)), L~ fixedDigits(L, 2)),
                       M = formatter("span",style = ~ ifelse(abs(M1) <= 0.05 & M1 < 0, "color:red",ifelse(abs(M1) <= 0.05, "color:blue", NA)), M~ fixedDigits(M, 2)),
                       N = formatter("span",style = ~ ifelse(abs(N1) <= 0.05 & N1 < 0, "color:red",ifelse(abs(N1) <= 0.05, "color:blue", NA)), N~ fixedDigits(N, 2)),
                       O = formatter("span",style = ~ ifelse(abs(O1) <= 0.05 & O1 < 0, "color:red",ifelse(abs(O1) <= 0.05, "color:blue", NA)), O~ fixedDigits(O, 2)),
                       P = formatter("span",style = ~ ifelse(abs(P1) <= 0.05 & P1 < 0, "color:red",ifelse(abs(P1) <= 0.05, "color:blue", NA)), P~ fixedDigits(P, 2)),
                       Q = formatter("span",style = ~ ifelse(abs(Q1) <= 0.05 & Q1 < 0, "color:red",ifelse(abs(Q1) <= 0.05, "color:blue", NA)), Q~ fixedDigits(Q, 2)),
                       R = formatter("span",style = ~ ifelse(abs(R1) <= 0.05 & R1 < 0, "color:red",ifelse(abs(R1) <= 0.05, "color:blue", NA)), R~ fixedDigits(R, 2)),
                       S = formatter("span",style = ~ ifelse(abs(S1) <= 0.05 & S1 < 0, "color:red",ifelse(abs(S1) <= 0.05, "color:blue", NA)), S~ fixedDigits(S, 2)),
                       T = formatter("span",style = ~ ifelse(abs(T1) <= 0.05 & T1 < 0, "color:red",ifelse(abs(T1) <= 0.05, "color:blue", NA)), T~ fixedDigits(T, 2)),
                       U = formatter("span",style = ~ ifelse(abs(U1) <= 0.05 & U1 < 0, "color:red",ifelse(abs(U1) <= 0.05, "color:blue", NA)), U ~ fixedDigits(U, 2)),
                       V = formatter("span",style = ~ ifelse(abs(V1) <= 0.05 & V1 < 0, "color:red",ifelse(abs(V1) <= 0.05, "color:blue", NA)), V ~ fixedDigits(V, 2)),
                       W = formatter("span",style = ~ ifelse(abs(W1) <= 0.05 & W1 < 0, "color:red",ifelse(abs(W1) <= 0.05, "color:blue", NA)), W ~ fixedDigits(W, 2)),
                       X = formatter("span",style = ~ ifelse(abs(X1) <= 0.05 & X1 < 0, "color:red",ifelse(abs(X1) <= 0.05, "color:blue", NA)), X ~ fixedDigits(X, 2)),
                       Y = formatter("span",style = ~ ifelse(abs(Y1) <= 0.05 & Y1 < 0, "color:red",ifelse(abs(Y1) <= 0.05, "color:blue", NA)), Y ~ fixedDigits(Y, 2)),
                       Z = formatter("span",style = ~ ifelse(abs(Z1) <= 0.05 & Z1 < 0, "color:red",ifelse(abs(Z1) <= 0.05, "color:blue", NA)), Z ~ fixedDigits(Z, 2)),
                       rsquared = rsquaredFormatter,
                       pvalue = pFormatter)
        # Removing unrequired formatters.
        for (i in 26:(k + 1))
            formatters[[i]] <- NULL
        # Removing unwanted variables (i.e., the variables that contain the p-values)
        p.values <- rep(FALSE, k)
        names(p.values) <- paste0(LETTERS[1:k],"1")
        formatters <- c(formatters, as.list(p.values))
        subtitle.format <- if (subtitle == "") NULL else tags$h5(class=".h5",
            style=paste0("color:", subtitleColour(), "; text-align:left; margin-top:2px; margin-bottom:0"), subtitle)
        title.format <- if (title == "") NULL else tags$h3(class=".h3",style=paste0("color:", titleColour(), "; text-align:left; margin-top:0px; margin-bottom:0"),title)
        tbl <- format_table(means,
                        col.names = column.names,
                        table.attr = paste('class = "table table-condensed"',
                                            'style = "margin:0; border-bottom: 2px solid; border-top: 2px solid;"'),
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
