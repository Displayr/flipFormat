context("utilities")

#' \code{"foo$fog$x"} with \code{"x"}



test_that("ReplacingEverythingBefore",
    {
        expect_equal(ReplacingEverythingBefore("foo$fog$x", "\\$"), "x")
        expect_equal(ReplacingEverythingBefore("foo$fog$x", "\\$", FALSE), "$x")
        expect_equal(ReplacingEverythingBefore("GiraffeDogCat", "Dog", FALSE), "DogCat")
        expect_equal(ReplacingEverythingBefore("GiraffeDogCat", "Dog", TRUE), "Cat")
        expect_equal(ReplacingEverythingBefore("a.a", "\\.", TRUE), "a")
        expect_equal(ReplacingEverythingBefore(c("a.a", "bbbbbbbbbbb.bbb"), "\\.", TRUE), c("a", "bbb"))
    })



lbls <- c("Q5.  Brand associations: feminine - Coke",
          "Q5.  Brand associations: feminine - Diet Coke",
          "Q5.  Brand associations: feminine - Coke Zero")
lbls2 <- c("Q5.  Brand associations: feminine - Coke",
          "Q5.  Brand associations: feminine - Diet Coke",
          "Q5.  Brand associations: health-conscious - Coke")
test_that("Extract prefix",
    {
        expect_equal(ExtractCommonPrefix(lbls)$common.prefix, "Q5.  Brand associations: feminine")
        expect_equal(ExtractCommonPrefix(lbls)$shortened.labels, c("Coke", "Diet Coke", "Coke Zero"))
        expect_equal(ExtractCommonPrefix(lbls2)$common.prefix, "Q5.  Brand associations")
        expect_equal(ExtractCommonPrefix(lbls2)$shortened.labels, c("feminine - Coke", "feminine - Diet Coke", "health-conscious - Coke"))
    })


test_that("TidyLabels",
    {
        z = "Q5.  Brand associations: feminine"
        expect_equal(TidyLabels(z), z)
        z = c(z, "Q5.  Brand associations: masculine")
        expect_equal(TidyLabels(z), c("Feminine", "Masculine"))
    })


test_that("DS-3053: Test CreateTransformedText function", {
    # Test redundant case for legacy purposes when transformed.text still existed.
    x <- sample(c(LETTERS, letters))
    basic.list <- list(transformed.text = x)
    expect_identical(CreateTransformedText(basic.list), x)
    # Create wordbag style list
    transformed.tokenized = structure(list(1:4, 5:10, 11:15, 16:17, 18:23, 24:28, 29:33, 16:17, 34:39,
                                           40:43, c(44L, 35L, 45L, 46L, 47L), 16:17, 48:49, 50:51,
                                           52:53, c(54L, 55L, 17L), c(56L, 52L, 57L), 1:4, 5:10, 11:15,
                                           16:17, 18:23, 24:28, 29:33, 16:17, 34:39, 40:43,
                                           c(44L, 35L, 45L, 46L, 47L), 16:17, 48:49, 50:51, 52:53,
                                           c(54L, 55L, 17L), c(56L, 52L, 57L), c(1L, 42L, 3L, 4L),
                                           c(5L,58L, 7L, 8L, 9L, 10L), c(59L, 12L, 13L, 14L, 15L),
                                           c(16L, 60L), c(18L, 61L, 62L, 63L, 22L, 23L), 24:28, 29:33,
                                           16:17, 34:39, 40:43, c(44L, 64L, 45L, 46L, 65L), 16:17,
                                           66:67, 50:51, 52:53, c(54L, 55L, 17L), c(56L, 52L, 57L )),
                                      .Label = c("raindrops", "roses", "whiskers", "kittens", "bright",
                                                 "copper", "kettles", "warm", "woolen", "mittens",
                                                 "brown", "paper", "packages", "tied", "strings",
                                                 "few", "favorite", "cream", "colored", "ponies",
                                                 "crisp", "apple", "streudels", "doorbells", "sleigh",
                                                 "bells", "schnitzel", "noodles", "wild", "geese",
                                                 "fly", "moon", "wings", "girls", "white", "dresses",
                                                 "blue", "satin", "sashes", "snowflakes", "stay",
                                                 "nose", "eyelashes", "silver", "winters", "melt",
                                                 "springs", "dog", "bites", "bee", "stings", "feel",
                                                 "sad", "simply", "remember", "dont", "bad", "cawpper",
                                                 "brun", "favourite", "coloured", "ponees", "krisp",
                                                 "whit", "sprang", "dawg", "bights"))
    text <- c("one i'll but she'd whom than keep our such just", "Bright copper kettles and warm woolen mittens",
              "Brown paper packages tied up with strings", "These are a few of my favorite things",
              "Cream colored ponies and crisp apple streudels", "Doorbells and sleigh bells and schnitzel with noodles",
              "Wild geese that fly with the moon on their wings", "These are a few of my favorite things",
              "Girls in white dresses with blue satin sashes", "Snowflakes that stay on my nose and eyelashes",
              "Silver white winters that melt into springs", "These are a few of my favorite things",
              "When the dog bites", "When the bee stings", "When I'm feeling sad",
              "I simply remember my favorite things", "And then I don't feel so bad",
              "Raindrops on roses and whiskers on kittens", "Bright copper kettles and warm woolen mittens",
              "Brown paper packages tied up with strings", "These are a few of my favorite things",
              "Cream colored ponies and crisp apple streudels", "Doorbells and sleigh bells and schnitzel with noodles",
              "Wild geese that fly with the moon on their wings", "These are a few of my favorite things",
              "Girls in white dresses with blue satin sashes", "Snowflakes that stay on my nose and eyelashes",
              "Silver white winters that melt into springs", "These are a few of my favorite things",
              "When the dog bites", "When the bee stings", "When I'm feeling sad",
              "I simply remember my favorite things", "And then I don't feel so bad",
              "Raindraps on noses and wiskers on kattens", "Bright cawpper kattles and warm wolen mattens",
              "Brun papier packagez tied up with string", "Thase are a few of my favourite thing",
              "Creem coloured ponees and krisp apple streudels", "Doorbells and sleigh bells and schnitzel with noodles",
              "Wild geese that fly with the moon on their wing", "These are a few of my favorite things",
              "Girl in white dress with blue satin sash", "Snowflakes that stay on my nose and eyelash",
              "Silver whit winters that melt into sprang", "These are a few of my favorite things",
              "When the dawg bights", "When the bee sting", "When I'm feeling sad",
              "I simply remember my favorite things", "And then I don't feel so bad")
    # Expected transformed text overall
    expected.transformed <- c("raindrops roses whiskers kittens", "bright copper kettles warm woolen mittens",
                              "brown paper packages tied strings", "few favorite", "cream colored ponies crisp apple streudels",
                              "doorbells sleigh bells schnitzel noodles", "wild geese fly moon wings",
                              "few favorite", "girls white dresses blue satin sashes", "snowflakes stay nose eyelashes",
                              "silver white winters melt springs", "few favorite", "dog bites",
                              "bee stings", "feel sad", "simply remember favorite", "dont feel bad",
                              "raindrops roses whiskers kittens", "bright copper kettles warm woolen mittens",
                              "brown paper packages tied strings", "few favorite", "cream colored ponies crisp apple streudels",
                              "doorbells sleigh bells schnitzel noodles", "wild geese fly moon wings",
                              "few favorite", "girls white dresses blue satin sashes", "snowflakes stay nose eyelashes",
                              "silver white winters melt springs", "few favorite", "dog bites",
                              "bee stings", "feel sad", "simply remember favorite", "dont feel bad",
                              "raindrops nose whiskers kittens", "bright cawpper kettles warm woolen mittens",
                              "brun paper packages tied strings", "few favourite", "cream coloured ponees krisp apple streudels",
                              "doorbells sleigh bells schnitzel noodles", "wild geese fly moon wings",
                              "few favorite", "girls white dresses blue satin sashes", "snowflakes stay nose eyelashes",
                              "silver whit winters melt sprang", "few favorite", "dawg bights",
                              "bee stings", "feel sad", "simply remember favorite", "dont feel bad")
    fav.wb <- structure(list(subset = rep(TRUE, 51),
                             transformed.tokenized = transformed.tokenized,
                             blank.after.transform = rep(FALSE, 51),
                             original.text = text),
                        class = "wordBag")
    my.subset <- rep(c(TRUE, FALSE), c(26, 25))
    fav.wb.with.subset <- fav.wb
    fav.wb.with.subset$subset <- my.subset
    fav.wb.with.subset$original.text <- text
    fav.wb.with.subset$blank.after.transform <- rep(FALSE, sum(fav.wb.with.subset$subset))
    expect_error(created.text <- CreateTransformedText(fav.wb), NA)
    expect_identical(created.text, expected.transformed)
    expect_error(subset.created.text <- CreateTransformedText(fav.wb.with.subset), NA)
    expect_equal(created.text[my.subset], subset.created.text[my.subset])
    expect_equal(subset.created.text[!my.subset], tolower(text[!my.subset]))
    # Simulate removal of tokens on non-filtered
    fav.wb.with.removed <- fav.wb
    inds.to.remove <- sample(c(TRUE, FALSE), size = length(text), replace = TRUE)
    fav.wb.with.removed$blank.after.transform <- logical(length(text))
    fav.wb.with.removed$blank.after.transform[inds.to.remove] <- TRUE
    fav.wb.with.removed$transformed.tokenized[inds.to.remove] <- replicate(sum(inds.to.remove), list(integer(0)))
    expect_error(output <- CreateTransformedText(fav.wb.with.removed), NA)
    expect_equal(output[inds.to.remove], rep("<NO_WORDS_REMAIN_AFTER_PROCESSING>", sum(inds.to.remove)))
    # Simulate removal of tokens on filtered
    fav.wb.subset.with.removed <- fav.wb.with.subset
    blanks <- logical(sum(my.subset))
    inds.to.remove <- sample(which(!blanks), size = length(blanks)/2, replace = TRUE)
    blanks[inds.to.remove] <- TRUE
    fav.wb.subset.with.removed$blank.after.transform <- blanks
    fav.wb.subset.with.removed$transformed.tokenized[my.subset][inds.to.remove] <- replicate(length(inds.to.remove), list(integer(0)))
    expect_error(output <- CreateTransformedText(fav.wb.subset.with.removed), NA)
    expect_equal(output[my.subset][blanks], rep("<NO_WORDS_REMAIN_AFTER_PROCESSING>", sum(blanks)))
    expect_equal(output[my.subset][!blanks], created.text[my.subset][!blanks])
    expect_equal(output[!my.subset], tolower(text[!my.subset]))
    # Expect error if an object not of wordbag class
    not.wordbag <- fav.wb
    class(not.wordbag) <- "list"
    expect_error(CreateTransformedText(not.wordbag),
                 "Transformed text can only be created from an object of class: wordBag",
                 fixed = TRUE)
    ## decodeNumericText checks
    expect_error(flipFormat:::decodeNumericText(1L),
                 "Unexpected input: text needs to be a character vector, list or a factor",
                 fixed = TRUE)
    # Expect decoded text to agree
    expect_equal(flipFormat:::decodeNumericText(fav.wb$transformed.tokenized),
                 strsplit(expected.transformed, " "))
    # Expect factors to be returned as character
    expect_equal(flipFormat:::decodeNumericText(factor(LETTERS[1:3])), LETTERS[1:3])
    # Expect text vector to be returned as is
    expect_equal(flipFormat:::decodeNumericText(LETTERS[1:3]), LETTERS[1:3])
})


test_that("iframeless Box is equivalent", {
    basic.code <- paste0("<h1>Foo</h1>",
                         "<h2>bar</h2>",
                         "<span style='color:red'>This is some red content.</span>", collapse = "")
    iframe.box <- rhtmlMetro::Box(basic.code, text.as.html = TRUE)
    iframeless.box <- boxIframeless(basic.code, text.as.html = TRUE)
    expect_true(attr(iframeless.box, "can-run-in-root-dom"))
    expect_null(attr(iframe.box, "can-run-in-root-dom"))
    attr(iframeless.box, "can-run-in-root-dom") <- NULL
    expect_equal(iframeless.box, iframe.box)
    # Same tests but toggling the options used in Box
    iframe.box.raw <- rhtmlMetro::Box(basic.code, text.as.html = FALSE)
    iframeless.box.raw <- boxIframeless(basic.code, text.as.html = FALSE)
    expect_true(attr(iframeless.box.raw, "can-run-in-root-dom"))
    expect_null(attr(iframe.box.raw, "can-run-in-root-dom"))
    attr(iframeless.box.raw, "can-run-in-root-dom") <- NULL
    expect_equal(iframeless.box.raw, iframe.box.raw)
    ## Blue background
    iframe.box.blue <- rhtmlMetro::Box(basic.code, text.as.html = FALSE, background.color = "blue")
    iframeless.box.blue <- boxIframeless(basic.code, text.as.html = FALSE, background.color = "blue")
    expect_true(attr(iframeless.box.blue, "can-run-in-root-dom"))
    expect_null(attr(iframe.box.blue, "can-run-in-root-dom"))
    attr(iframeless.box.blue, "can-run-in-root-dom") <- NULL
    expect_equal(iframeless.box.blue, iframe.box.blue)
})
