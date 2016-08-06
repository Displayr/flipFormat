ft <- "Yo! This footer specifically designed
      to communicate important information.
Since it is so important, it will of course
extend over many lines.  In fact, on narrow tables,
it might take >3.  On wide tables, it might only
require one.  Feel free to adjust the width,
and the importance and significance does not
go away."



coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
rownames(coef.matrix)[1] <- "Big dog"

PrettyRegressionTable(coef.matrix, TRUE, ft, title = "My awesome regression", subtitle = "Big brown dog")
