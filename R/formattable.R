# Set the number of decimals
fixedDigits <- function(x, n = 2) {
    formatC(x, digits = n, format = "f")
}
# Formats r-squareds
rsquaredFormatter <- formatter(.tag = "span", style = function(x) style(
    display = "inline-block", direction = "rtl", `border-radius` = "4px", `padding-right` = "0px",
    `background-color` = "#DDDDDD", width = percent(x / max(x))), ~ fixedDigits(rsquared, 2))

# Format p-values.
pFormatter <- function(p, p.cutoff)
{
    formatter("span",
    style = p ~ ifelse(p <= p.cutoff, style(font.weight = "bold"), NA),
    p ~ {
            p.formatted <- fixedDigits(p, 3)
            p.formatted <- gsub(x = p.formatted, pattern="^(-?)0", replacement="\\1")
            p.formatted[p < 0.001] <- "< .001"
            p.formatted
        }
    )
}

# Z Heat Map-like color scale
.zcolorScale <- function(x, min = -5, max = 5)
{
    result <- character(length(x))
    result[x >= 0] <- csscolor(gradient(c(0, max, x[x >= 0]), "white", "#80B4F4"))[-2:-1]
    result[x < 0] <- csscolor(gradient(c(min, 0, x[x < 0]), "#FB9080", "white"))[-2:-1]
    result
}

# Shades means according to their z-scores
heatMapZ <- function(x)
{
    txt <- sprintf("~ style(display = \"block\", padding = \"0 4px\", `border-radius` = \"4px\",
                   `font-weight` = ifelse(abs(%s1) <= 0.05, \"bold\", NA),
                   `background-color` = .zcolorScale(%s2))", x, x, x, x, x)
    formatter("span", style = eval(parse(text = txt)),
              eval(parse(text = sprintf("%s~ fixedDigits(%s, 2)", x, x))))
}
