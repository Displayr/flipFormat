title <- "Automatic Categorization: What don't you like about Tom Cruise?  That is, what is it about Tom Cruise that you dislike?"

category.accuracy <- c(`Religion and Scientology` = 1, Nothing = 1, Ego = 1)

cv.metrics <- structure(c(50, 100, 111, 61, 0.8326531, 0.9186441, 0.7457670, 0.846862, 0.7806129, 0.9126599),
                        .Dim = c(2L, 5L),
                        .Dimnames = list(NULL,
                                         c("Estimation sample size", "Validation sample size",
                                           "Accuracy", "Kappa", "F1")))

footer <- paste0("n = 299 cases were used in the text processing of a total of 300;",
                 " cases containing missing values have been excluded;",
                 " existing categorization classified 107 cases into 1 of 3 categories;",
                 " training sample performance - Accuracy: 100.0%; Cohen's Kappa: 1.00; F1:1.00;")

missing <-rep(FALSE, 300)
missing[115] <- TRUE

observed.counts <- c(`Religion and Scientology` = 62, Nothing = 49, Ego = 22)
overall.metrics <- c(Accuracy = 1, Kappa = 1, F1 = 1)

# The variable below gives the elements to populate the examples in the expandable arrows.
# For a TextClassifier run on the Tom Cruise data, this would produce much more examples
# A minimal exampleis given here with 2, 3 and 4 examples (and a single missing)
text.raw.by.categorization <- structure(
    list(c(`4` = "RELIGOUS VIEWS", `14` = "scientology"),
    c(`1` = "Nothing I really like him.", `5` = "Nothing", `11` = "nothing"),
    c(`13` = "his ego", `26` = "he seems really weird and kinda crazy",
      `61` = "I just think the media makes him out to be crazy. The Scientology part is crazy"),
    c(`115` = NA_character_)),
    .Names = c("Religion and Scientology", "Nothing", "Ego", NA))

weighted.sizes <- structure(c(87L, 58L, 26L),
                            .Names = c("Religion and Scientology", "Nothing", "Ego"))

examples <- c(`299` = "scientology",
              `168` = "Nothing in particular", `252` = "Crazy man, religious beliefs.")

test_that("Widget output check", {
    expect_error(result <- TextClassifierWidget(observed.counts, weighted.sizes, category.accuracy,
                                                examples, cv.metrics = NULL, text.raw.by.categorization,
                                                missing, title, footer),
                 NA)
    expect_is(result, "htmlwidget")
    expect_is(result, "rhtmlMetro")
    expect_error(result <- TextClassifierWidget(observed.counts, weighted.sizes, category.accuracy,
                                                examples, cv.metrics = cv.metrics, text.raw.by.categorization,
                                                missing, title, footer),
                 NA)
    expect_is(result, "htmlwidget")
    expect_is(result, "rhtmlMetro")
})
