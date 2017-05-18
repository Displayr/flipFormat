context("MaxDiffTable")

probabilities <- structure(c(0.611525681882981, 0.0610316269064962, 0.0196495417610775,
            0.109770173219371, 0.0293995220816906, 0.053582140249799, 0.0542225282202209,
            0.0229627602502246, 0.018320548748006, 0.019535476680133, 0.0296463742092417,
            0.118585164099962, 0.0455330492719019, 0.188428307189674, 0.0753169517641536,
            0.213817719826495, 0.151650357780703, 0.0598285025845872, 0.041148975345747,
            0.0760445979275344), .Dim = c(10L, 2L), .Dimnames = list(c("Apple",
                                                                       "Microsoft", "IBM", "Google", "Intel", "Samsung", "Sony", "Dell",
                                                                       "Yahoo", "Nokia"), c("Class 1", "Class 2")))
col.labels <- c("Class 1 (%)<br>Size: 60.5%", "Class 2 (%)<br>Size: 39.6%")

expect_error(MaxDiffTable(probabilities, col.labels, title = "Max-diff",
                                   subtitle = "subtitle here", footer = "footer here"), NA)
