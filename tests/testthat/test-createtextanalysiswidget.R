context("CreateTextAnalysisWidget")

findInstDirFile <- function(file)
{
  file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
            file)
}

raw.and.normalized.text <- list(`Original Text` = c("Aim to higlight ai. Raindrops on roses and whiskers on kittens",
  "Bright copper kettles and warm woolen mittens", "Brown paper packages tied up with strings",
  "These are a few of my favorite things", "Cream colored ponies and crisp apple streudels",
  "Doorbells and sleigh bells and schnitzel with noodles", "Wild geese that fly with the moon on their wings",
  "These are a few of my favorite things", "Girls in white dresses with blue satin sashes",
  "Snowflakes that stay on my nose and eyelashes", "Silver white winters that melt into springs",
  "These are a few of my favorite things", "When the dog bites!",
  "When the bee stings", "When I'm feeling sad", "I simply remember my favorite things",
  "And then I don't feel so bad", "Raindrops on roses and whiskers on kittens",
  "Bright copper kettles and warm woolen mittens", "Brown paper packages tied up with strings",
  "These are a few of my favorite things", "Cream colored ponies and crisp apple streudels",
  "Doorbells and sleigh bells and schnitzel with noodles", "Wild geese that fly with the moon on their wings",
  "These are a few of my favorite things", "Girls in white dresses with blue satin sashes",
  "Snowflakes that stay on my nose and eyelashes", "Silver white winters that melt into springs",
  "These are a few of my favorite things", "When the dog bites!",
  "When the bee stings", "When I'm feeling sad", "I simply remember my favorite things",
  "And then I don't feel so bad", "Raindraps on noses and wiskers on kattens",
  "Bright cawpper kattles and warm wolen mattens", "Brun papier packagez tied up with string",
  "Thase are a few of my favourite thing", "Creem coloured ponees and krisp apple streudels",
  "Doorbells and sleigh bells and schnitzel with noodles", "Wild geese that fly with the moon on their wing",
  "These are a few of my favorite things", "Girl in white dress with blue satin sash",
  "Snowflakes that stay on my nose and eyelash", "Silver whit winters that melt into sprang",
  "These are a few of my favorite things", "When the dawg bights",
  "When the bee sting", "When I'm feeling sad", "I simply remember my favorite things",
  "And then I don't feel so bad"), `Transformed Text` = list(c("ai", "raindrops",
  "roses", "hair on dogs"), c("bright", "copper", "kettles", "warm",
  "woolen", "mittens"), c("brown", "paper", "packages", "tied",
  "strings"), c("few", "your majesty", "faves"), c("cream", "colored",
  "ponies", "crisp", "apple", "streudels"), c("doorbells", "sleigh",
  "bells", "schnitzel", "noodles"), c("wild", "geese", "fly", "moon",
  "wings"), c("few", "your majesty", "faves"), c("girls", "white",
  "dresses", "red wool", "sashes"), c("snowflakes", "stay", "your majesty",
  "nose", "eyelashes"), c("silver", "white", "winters", "melt",
  "springs"), c("few", "your majesty", "faves"), c("dog", "bites!"
  ), c("bee", "stings"), c("feel", "sad"), c("simply", "remember",
  "your majesty", "faves"), c("dont", "feel", "bad"), c("raindrops",
  "roses", "hair on dogs"), c("bright", "copper", "kettles", "warm",
  "woolen", "mittens"), c("brown", "paper", "packages", "tied",
  "strings"), c("few", "your majesty", "faves"), c("cream", "colored",
  "ponies", "crisp", "apple", "streudels"), c("doorbells", "sleigh",
  "bells", "schnitzel", "noodles"), c("wild", "geese", "fly", "moon",
  "wings"), c("few", "your majesty", "faves"), c("girls", "white",
  "dresses", "red wool", "sashes"), c("snowflakes", "stay", "your majesty",
  "nose", "eyelashes"), c("silver", "white", "winters", "melt",
  "springs"), c("few", "your majesty", "faves"), c("dog", "bites!"
  ), c("bee", "stings"), c("feel", "sad"), c("simply", "remember",
  "your majesty", "faves"), c("dont", "feel", "bad"), c("raindrops",
  "nose", "wiskers", "kattens"), c("bright", "cawpper", "kattles",
  "warm", "woolen", "mittens"), c("brun", "paper", "packages",
  "tied", "strings"), c("few", "your majesty", "favourite"), c("cream",
  "coloured", "ponees", "krisp", "apple", "streudels"), c("doorbells",
  "sleigh", "bells", "schnitzel", "noodles"), c("wild", "geese",
  "fly", "moon", "wings"), c("few", "your majesty", "faves"), c("girls",
  "white", "dresses", "red wool", "sashes"), c("snowflakes", "stay",
  "your majesty", "nose", "eyelashes"), c("silver", "whit", "winters",
  "melt", "sprang"), c("few", "your majesty", "faves"), c("dawg",
  "bights"), c("bee", "stings"), c("feel", "sad"), c("simply",
  "remember", "your majesty", "faves"), c("dont", "feel", "bad"
  )), `Row Indices` = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
  12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L,
  26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L,
  42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 52L),
  `Variable Start Indices` = c(`Variable 1` = 1, `Variable 2` = 42))

n.gram.frequencies <- structure(list(Words = structure(1:70, .Label = c("ai", "apple", "bad",
"bee", "bells", "bights", "bites!", "bright", "brown", "brun",
        "cawpper", "colored", "coloured", "copper", "cream", "crisp",
        "dawg", "dog", "dont", "doorbells", "dresses", "eyelashes", "faves",
        "favourite", "feel", "few", "fly", "geese", "girls", "hair on dogs",
        "kattens", "kattles", "kettles", "krisp", "melt", "mittens",
        "moon", "noodles", "nose", "packages", "paper", "ponees", "ponies",
        "raindrops", "red wool", "remember", "roses", "sad", "sashes",
        "schnitzel", "silver", "simply", "sleigh", "snowflakes", "sprang",
        "springs", "stay", "stings", "streudels", "strings", "tied",
        "warm", "whit", "white", "wild", "wings", "winters", "wiskers",
        "woolen", "your majesty"), class = "factor"), Frequencies = c(1, 3,
          3, 3, 3, 1, 2, 3, 2, 1, 1, 2, 1, 2, 3, 2, 1, 2, 3, 3, 3, 3, 11,
          1, 6, 9, 3, 3, 3, 2, 1, 1, 2, 1, 3, 3, 3, 3, 4, 3, 3, 1, 2, 3,
          3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 1, 5, 3,
          3, 3, 1, 3, 15)), class = "data.frame", row.names = c(NA, -70L
          ))

token.substitution <- structure(c("ai", "a", "and", "apple", "are", "bad", "bee", "bells",
          "bights", "bites!", "blue satin", "bright", "brown", "brun", "cawpper",
          "colored", "coloured", "copper", "cream", "creem", "crisp", "dawg",
          "dog", "don't", "doorbells", "dress", "dresses", "eyelash", "eyelashes",
          "favorite things", "favourite", "feel", "feeling", "few", "fly",
          "geese", "girl", "girls", "i", "im", "in", "into", "kattens",
          "kattles", "kettles", "krisp", "mattens", "melt", "mittens",
          "moon", "my", "noodles", "nose", "noses", "of", "on", "packages",
          "packagez", "paper", "papier", "ponees", "ponies", "raindraps",
          "raindrops", "remember", "roses", "sad", "sash", "sashes", "schnitzel",
          "silver", "simply", "sleigh", "snowflakes", "so", "sprang", "springs",
          "stay", "sting", "stings", "streudels", "string", "strings",
          "thase", "that", "the", "their", "then", "these", "thing", "tied",
          "up", "warm", "when", "whiskers on kittens", "whit", "white",
          "wild", "wing", "wings", "winters", "wiskers", "with", "wolen",
          "woolen", "hair on dogs", "red wool", "faves", "your majesty",
          "ai", "", "", "apple", "", "bad", "bee", "bells", "bights", "bites!",
          "red wool", "bright", "brown", "brun", "cawpper", "colored",
          "coloured", "copper", "cream", "cream", "crisp", "dawg", "dog",
          "dont", "doorbells", "dresses", "dresses", "eyelashes", "eyelashes",
          "faves", "favourite", "feel", "feel", "few", "fly", "geese",
          "girls", "girls", "", "", "", "", "kattens", "kattles", "kettles",
          "krisp", "mittens", "melt", "mittens", "moon", "your majesty",
          "noodles", "nose", "nose", "", "", "packages", "packages", "paper",
          "paper", "ponees", "ponies", "raindrops", "raindrops", "remember",
          "roses", "sad", "sashes", "sashes", "schnitzel", "silver", "simply",
          "sleigh", "snowflakes", "", "sprang", "springs", "stay", "stings",
          "stings", "streudels", "strings", "strings", "", "", "", "",
          "", "", "", "tied", "", "warm", "", "hair on dogs", "whit", "white",
          "wild", "wings", "wings", "winters", "wiskers", "", "woolen",
          "woolen", "hair on dogs", "red wool", "faves", "your majesty"
), .Dim = c(109L, 2L), .Dimnames = list(NULL, c("old", "new")))

footer <- "n = 51 cases were used in the text processing;"

# Manually, you also want to check that 'ai' (not "Aim") is highlighted in line 1
# And 'bites!' is highlighted in line 13
test_that("Text analysis output",
{
    result <- CreateTextAnalysisWidget(raw.and.normalized.text,
                                       n.gram.frequencies,
                                       token.substitution, footer)
    expect_is(result, "htmlwidget")
})

test_that("Raw text diagnostics",
{
    # for split text highlighting
    raw.and.normalized.text$`Split categories info` <- list(list(to.be.split = "Cream colored ponies",
                                                                 categories = c("cream", "colored", "ponies"),
                                                                 rows = 5,
                                                                 raw.text = "Cream colored ponies and crisp apple streudels",
                                                                 raw.text.var.num = 1, raw.text.case.num = 5,
                                                                 is.max.exceeded = FALSE, n.omitted.rows = 0))

    category.examples <- lapply(rep(letters, 4)[1:70], function(x) x)
    category.examples[[1]] <- c("multiple", "lines")

    load(findInstDirFile("text.analysis.diagnostics.rda"))
    result <- CreateTextAnalysisWidget(raw.and.normalized.text,
                                       n.gram.frequencies,
                                       token.substitution,
                                       footer,
                                       text.analysis.diagnostics,
                                       category.examples = category.examples)
})
