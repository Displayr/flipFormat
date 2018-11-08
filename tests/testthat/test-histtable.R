context("HistTable")

dat <- data.frame(A=rpois(500,5), B=rpois(500,50), C=rpois(500,20))
expect_error(HistTable(dat,
                       title = "Histograms",
                       subtitle = "subtitle here",
                       footer = "footer here"), NA)

dat <- data.frame(A=rnorm(500,0), B=rnorm(500,0), C=rnorm(500,0))
expect_error(HistTable(dat,
                       title = "Histograms",
                       subtitle = "subtitle here",
                       footer = "footer here",
                       show.tooltips = FALSE,
                       color.negative = TRUE,
                       bin.min = -5,
                       bin.max = 5,
                       bin.size = 0.5), NA)

dat <- data.frame(A=rnorm(100,0), B=rnorm(100,0), C=rnorm(100,0))
names(dat) <- c("A", "B", "C")
class.memberships <- sample(1:6, 500, replace = TRUE)
prior.columns <- data.frame("Attribute" = c("xxxxxx xxxxxxx xxxxxxx xxxxxx xxxxxxx xxxxxxx xxxxxx xxxxxxx xxxxxxx",
                                            "y", "z"), "Level" = c("u", "v", "w"))
expect_error(HistTable(dat,
                       class.memberships = class.memberships,
                       class.sizes = c(0.207165110577296, 0.15009896031433, 0.142735929108374,
                                       0.207165110577296, 0.15009896031433, 0.142735929108374),
                       title = "Histograms",
                       subtitle = "subtitle here",
                       footer = "footer here",
                       show.tooltips = FALSE,
                       color.negative = TRUE,
                       bin.min = -5,
                       bin.max = 5,
                       bin.size = 0.5,
                       prior.columns = prior.columns,
                       show.row.names = FALSE,
                       row.lines.to.thicken = 2), NA)
