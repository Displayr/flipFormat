context("Experimental design")

require("flipChoice")

no.prior <- structure(c("Brand", "Hershey", "Dove", "Godiva", "Lindt", "Price",
               "$0.99", "$1.49", "$1.99", "$2.49", "Cocoa strength", "70%",
               "Dark", "Milk", "White", "Sugar", "Standard", "50% reduced sugar",
               "Sugar free", "", "Origin", "USA", "Switzerland", "Belgium",
               "Belgian (Single origin Venezuelan Criollo beans)", "Nuts", "Almonds",
               "Hazelnuts", "No", "", "Ethical", "Fair trade", "BLANK", "",
               ""), .Dim = c(5L, 7L))

has.prior <- structure(c("Brand", "Hershey", "Dove", "Godiva", "Lindt", "mean",
                         "1", "2", "3", "4", "Price", "$0.99", "$1.49", "$1.99", "$2.49",
                         "Cocoa strength", "70%", "Dark", "Milk", "White", "Sugar", "Standard",
                         "50% reduced sugar", "Sugar free", "", "Origin", "USA", "Switzerland",
                         "Belgium", "Belgian (Single origin Venezuelan Criollo beans)",
                         "Nuts", "Almonds", "Hazelnuts", "No", "", "Ethical", "Fair trade",
                         "BLANK", "", ""), .Dim = c(5L, 8L))

test_that("ChoiceModelDesign print, none alternative",
{
    skip_if_not_installed("flipChoice")
    cmd <- suppressWarnings(ChoiceModelDesign(design.algorithm = "Shortcut",
                                attribute.levels = has.prior,
                                n.questions = 6,
                                n.versions = 3,
                                none.alternatives = 1,
                                alternatives.per.question = 4,
                                seed = 1))
    out <- CreateChoiceModelDesignWidget(cmd)
    expect_is(out, "htmlwidget")
    expect_equal(attr(out, "ChartData"), cmd$labeled.design)
})

test_that("ChoiceModelDesign print, p.p with constant attributes",
{
    skip_if_not_installed("flipChoice")
    cmd <- suppressWarnings(ChoiceModelDesign(design.algorithm = "Partial profiles",
                                attribute.levels = has.prior,
                                n.questions = 6,
                                n.versions = 3,
                                none.alternatives = 0,
                                n.constant.attributes = 1,
                                alternatives.per.question = 2,
                                seed = 1))
    out <- print(cmd)
    expect_is(out, "htmlwidget")

    ## some diagnostics not available for partial profiles
    ## only one row in stats table
    tfile <- tempfile()
    cata <- createCata(tfile)
    kt <- addStatistics(cmd, 2, 2, cata)
    m <- regexec("<tr>", readLines(tfile), fixed = TRUE)
    expect_length(unlist(regmatches(readLines(tfile), m)), 1)
})

test_that("ChoiceModelDesign print, 1 version with prior",
{
    skip_if_not_installed("flipChoice")
    cmd <- suppressWarnings(ChoiceModelDesign(design.algorithm = "Shortcut",
                                attribute.levels = has.prior,
                                n.questions = 20,
                                n.versions = 1,
                                none.alternatives = 0,
                                alternatives.per.question = 6,
                                seed = 1))
    out <- print(cmd)
    expect_is(out, "htmlwidget")

    ## should have all diagnostics, 3 rows in stats table
    tfile <- tempfile()
    cata <- createCata(tfile)
    kt <- addStatistics(cmd, 2, 2, cata)
    m <- regexec("<tr>", readLines(tfile), fixed = TRUE)
    expect_length(unlist(regmatches(readLines(tfile), m)), 3L)
})

test_that("ChoiceModelDesign print shows zero prior correctly",
{
    ## The unit test "Utility neutral integrated algorithm" in
    ## test-partialprofilesdesign.R tests that "prior"
    ## element of ChoiceModelDesign object
    ## is always NULL if none is supplied by the user.
    ## this test just checks that the user can still
    ## specify a zero prior and have that included
    ## in the prior output
    skip_if_not_installed("flipChoice")
    has.prior <- structure(c("Brand", "Hershey", "Dove", "Godiva", "Lindt", "mean",
                         "0", "0", "0", "0", "Price", "$0.99", "$1.49", "$1.99", "$2.49",
                         "Cocoa strength", "70%", "Dark", "Milk", "White", "Sugar", "Standard",
                         "50% reduced sugar", "Sugar free", "", "Origin", "USA", "Switzerland",
                         "Belgium", "Belgian (Single origin Venezuelan Criollo beans)",
                         "Nuts", "Almonds", "Hazelnuts", "No", "", "Ethical", "Fair trade",
                         "BLANK", "", ""), .Dim = c(5L, 8L))
    cmd <- suppressWarnings(ChoiceModelDesign(design.algorithm = "Partial profiles",
                                attribute.levels = has.prior,
                                n.questions = 10,
                                n.versions = 5,
                                none.alternatives = 0,
                                alternatives.per.question = 6,
                                seed = 1))
    expect_true(all(cmd$prior == 0))

    ## As of DS-2311, prior is not stripped if alg isnt one of efficient, Modfed, or p.p
    cmd <- suppressWarnings(ChoiceModelDesign(design.algorithm = "Random",
                                attribute.levels = has.prior,
                                n.questions = 10,
                                n.versions = 5,
                                none.alternatives = 0,
                                alternatives.per.question = 6,
                                seed = 1))
    expect_true(all(cmd$prior == 0))
})

test_that("ChoiceModelDesign print prior with sd given",
{
    skip_if_not_installed("flipChoice")
    prior <- matrix(c(-0.8, -0.8, -0.8, -0.8, -0.8, -0.8, rep(0.4, 6)), ncol = 2)
    attr.list <- list(A1 = 1:2, A2 = 1:2, A3 = 1:2, A4 = 1:2,
                      A5 = 1:2, A6 = 1:2)
    ## make sure printing with
    out <- makePriorTable(prior, attr.list, 2, 2)
    expect_true(grepl("class=\"table-two-stat\"", out, fixed = TRUE))
})

test_that("CMD print when no standard errors; DS-2881",
{
    al <- structure(c("MECHANISM OF ACTION", "MOA 1", "MOA 2",
                      "MOA 3", "", "", "", "ROA", "ROA 1", "ROA 2",
                      "ROA 3", "", "", "", "DOSING FREQUENCY",
                      "Twice weekly", "Every 2 weeks",
                      "Every 2 weeks with option to\n  titrate to every 4 weeks",
                      "Every 4 weeks", "Once daily", "Twice daily",
                      "PRIMARY\n  ENDPOINT", "44%", "65%", "84%",
                      "87%", "52%", "", "SAFETY", "Safety 1",
                      "Safety 2", "", "", "", ""), .Dim = c(7L, 5L))
    p <- structure(c("MOA 1", "MOA 2", "MOA 3", "MOA 1", "MOA 1", "MOA 2",
                     "MOA 3", "MOA 2", "MOA 3", "", "", "", "", "",
                     "", "", "", "", "", "", "", "", "", "", "", "",
                     "", "", "", "ROA 3", "ROA 1", "ROA 1", "", "",
                     "", "", "", "", "ROA 1", "ROA 3", "ROA 2",
                     "ROA 1", "ROA 3", "ROA 1", "ROA 1", "", "", "",
                     "", "", "", "", "", "", "", "ROA 3", "ROA 2",
                     "ROA 1", "", "", "", "Once daily", "Twice daily",
                     "", "", "", "", "", "", "", "", "", "", "",
                     "Every 2 weeks",
                     "Every 2 weeks with option to\n  titrate to every 4 weeks",
                     "Every 4 weeks", "Once daily", "Twice daily",
                     "Every 2 weeks",
                     "Every 2 weeks with option to\n  titrate to every 4 weeks",
                     "Every 4 weeks", "Once daily", "Twice daily", "",
                     "", "", "", "", "", "", "", "52%", "52%", "84%",
                     "84%", "44%", "52%", "52%", "84%", "84%", "65%",
                     "87%", "84%", "84%", "84%", "84%", "84%", "52%",
                     "52%", "52%", "52%", "52%", "", "", "", "", "",
                     "", "", "", "", "", "", "", "", "", "", "", "",
                     "", "", "", "", "", "", "", "", "", "", "", "",
                     "Safety 1", "Safety 2", "Safety 2"), .Dim = c(29L,
                                                                   5L))
    expect_warning(cmd <- ChoiceModelDesign(design.algorithm = "Balanced overlap",
                                            prohibitions = p, attribute.levels = al,
                                            alternatives.per.question= 3,
                                            n.questions = 6, n.versions = 10),
                   "Standard errors cannot be calculated")
    expect_error(print(cmd), NA)
})
