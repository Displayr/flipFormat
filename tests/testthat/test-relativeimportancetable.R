context("RelativeImportanceTable")

ria <- structure(list(raw.importance = c(0.118944441811052, 0.0739776637555959, 0.0877737045218754, 0.126469123604018, 0.0451172891545317, 0.0380904799320387),
               importance = c(24.2559263876135, 15.0860077113467, 17.8993863288946, 25.7904085784698, 9.2006118812969, 7.76765911237855),
               standard.errors = c(0.0184429176092881, 0.0149627789928388, 0.0156164385208526, 0.0214172610443465, 0.0117435644978904, 0.0107317819892912),
               df = c(586.115071291117, 381.952936900779, 692.888967744458, 112.642034036791, 352.875261715552, 370.654202605551),
               statistics = c(6.44932891481065, 4.94411257367377, 5.62059680923224, 5.90500920459211, 3.84187349272331, 3.54931547901809),
               p.values = c(2.35165037485065e-10, 1.14763860150951e-06, 2.75917778584525e-08, 3.80419427846665e-08, 0.000144754048428863, 0.000435905595780471),
               statistic.name = "t"),
          .Names = c("raw.importance", "importance", "standard.errors", "df", "statistics", "p.values", "statistic.name"))

row.labels <- c("Fees", "Interest", "Phone", "Branch", "Online", "ATM")

expect_error(print(RelativeImportanceTable(ria, row.labels, title = "Relative Importance Analysis",
                                           subtitle = "Overall", footer = "Footer")), NA)
