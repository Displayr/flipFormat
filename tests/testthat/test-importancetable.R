context("ImportanceTable")

ria <- structure(list(raw.importance = c(0.118944441811052, 0.0739776637555959, 0.0877737045218754, 0.126469123604018, 0.0451172891545317, 0.0380904799320387),
               importance = c(24.2559263876135, 15.0860077113467, 17.8993863288946, 25.7904085784698, 9.2006118812969, -7.76765911237855),
               standard.errors = c(0.0184429176092881, 0.0149627789928388, 0.0156164385208526, 0.0214172610443465, 0.0117435644978904, 0.0107317819892912),
               df = c(586.115071291117, 381.952936900779, 692.888967744458, 112.642034036791, 352.875261715552, 370.654202605551),
               statistics = c(6.44932891481065, 4.94411257367377, 5.62059680923224, 5.90500920459211, 3.84187349272331, 3.54931547901809),
               p.values = c(2.35165037485065e-10, 1.14763860150951e-06, 2.75917778584525e-08, 3.80419427846665e-08, 0.000144754048428863, 0.000435905595780471),
               statistic.name = "t"),
          .Names = c("raw.importance", "importance", "standard.errors", "df", "statistics", "p.values", "statistic.name"))

row.labels <- c("Fees", "Interest", "Phone", "Branch", "Online", "ATM")

expect_error(print(ImportanceTable(ria, row.labels, title = "Relative Importance Analysis",
                                   subtitle = "Overall", footer = "Footer",
                                   output.type = "Relative Importance Analysis")), NA)

# Correlation output test

driver.analysis.output <- list(importance = c(X1 = 30.7615645939375, X2 = 61.4742721786786, X3 = 7.76416322738386),
                               raw.importance.score = c(X1 = 0.444103898829528, X2 = 0.88750245030252, X3 = 0.112091020269807),
                               statistics = c(X1 = 4.05959601335654, X2 = 7.71002892426731, X3 = 1.27220209511662),
                               standard.errors = c(X1 = 0.109396082114667, X2 = 0.11511013240289, X3 = 0.0881078727193356),
                               sample.size = c(X1 = 100, X2 = 100, X3 = 100),
                               p.values = c(X1 = 9.89747713150152e-05, X2 = 1.04871955760437e-11, X3 = 0.206311752325173))
row.labels <- c("Apples", "Oranges", "Grapes")
title <- "Correlation (Linear Regression): Sweetness by Fruit"
footer <- "n = 100 cases used in estimation; R-squared: 0.9909; multiple comparisons correction: None;"
output.type <- "Correlation"

expect_error(print(ImportanceTable(driver.analysis.output, row.labels, title, subtitle = "", footer, p.cutoff = 0.05, output.type)),
             NA)

# Jaccard output test

driver.analysis.output <- list(importance = c(Oranges = 24.6491130702577, Apples = 44.0831595528863, Grapes = 31.267727376856),
                              raw.importance.score = c(Oranges = 0.426470588235294, Apples = 0.76271186440678, Grapes = 0.540983606557377),
                              standard.errors = c(Oranges = 0.060276820521488, Apples = 0.0556640318583818, Grapes = 0.0641244474358982),
                              sample.size = c(Oranges = 100, Apples = 100, Grapes = 100),
                              p.values = c(Oranges = 0.0748074008481348, Apples = 2.24815464755804e-11, Grapes = 0.00041056331219862))
row.labels <- c("Oranges", "Apples", "Grapes")
title <- "Jaccard Coefficient (Linear Regression): Super Y by Fruit"
footer <- "n = 100 cases used in estimation; R-squared: 0.7566; multiple comparisons correction: None;"
output.type <- "Jaccard Coefficient"
expect_error(print(ImportanceTable(driver.analysis.output, row.labels, title, subtitle = "", footer, p.cutoff = 0.05, output.type)),
             NA)
