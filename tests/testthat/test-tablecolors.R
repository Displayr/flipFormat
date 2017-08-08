context("Tablecolors")

test_that("z scores as colors", {
    z = -10:10
    pie(abs(z), clockwise=TRUE, labels=LETTERS[1:11], border="white", col= ZScoresAsColors(z))
})

#
#     n <- colorRampPalette(col2rgb(c("#FA614B00", "#3E7DCC00")))
#     n <- n(101)
#     positive.colors <- colorRampPalette(c(non.significant.color, positive.color))
#     positive.colors <- positive.colors(101)
#
#
