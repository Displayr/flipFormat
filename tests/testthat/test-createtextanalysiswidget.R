findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipFormat", mustWork = TRUE),
              file)
}

load(findInstDirFile("text.analysis.raw.text.rda"))
load(findInstDirFile("text.analysis.normalized.text.rda"))

final.tokens <- c("about", "aware", "better", "bit", "brand", "calories", "coca",
  "coke", "coke+zero", "cola", "concerned", "conscious", "depends",
  "diet", "difference", "dont", "drink", "drinkers", "feel", "females",
  "flavour", "free", "health", "idea", "know", "less", "light",
  "like", "little", "males", "marketing", "max", "more", "most",
  "no", "none", "not", "nothing", "people", "pepsi", "personal",
  "prefer", "probably", "real", "some", "sugar", "sweet", "taste",
  "tend", "think", "traditional", "trying", "want", "watching",
  "weight", "whereas", "younger", "zero", "class")

final.counts <- c(13, 6, 18, 6, 11, 9, 24, 69, 47, 31, 7, 34, 6, 64, 31, 28,
  119, 19, 5, 5, 6, 5, 25, 6, 20, 11, 25, 38, 6, 5, 5, 35, 56,
  7, 23, 5, 22, 11, 101, 74, 11, 22, 10, 6, 9, 42, 8, 89, 15, 31,
  5, 9, 12, 7, 42, 5, 5, 16, 1)

ind <- rank(-final.counts, ties.method = "first")
final.tokens[ind] <- final.tokens
final.counts[ind] <- final.counts

raw.and.normalized.text <- data.frame(raw.text = text.analysis.raw.text,
                              normalized.text = text.analysis.normalized.text,
                              stringsAsFactors = FALSE)

n.gram.frequencies <- data.frame(n.grams = final.tokens, frequencies = final.counts,
                                 stringsAsFactors = FALSE)

footer <- "n = 321 cases used to process the text of a total of 327; 6 cases are blank before transformation; 33 cases are blank after transformation."

test_that("Text analysis output",
{
    result <- CreateTextAnalysisWidget(raw.and.normalized.text, n.gram.frequencies, footer)
    expect_is(result, "htmlwidget")
})
