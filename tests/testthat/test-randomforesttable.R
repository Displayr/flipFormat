context("RandomForestTable")

importance <- structure(c(0.0128398377610784, 0.0527666380482331, 0.0344499196583041,
            -0.00923795419828568, 0.0417222050060869, 0.011247912840829,
            -0.00103723773034118, -0.000273034117861704, -0.000371428571428571,
            0.000878290869865284, 0.042634479583465, 0.0200207431410086,
            1.38706663292142, 7.69797543841583, 3.98114490374249), .Dim = c(3L,
                                                                            5L), .Dimnames = list(c("q5: feminine - Coke Zero", "q5: health-conscious - Coke Zero", "q5: innocent - Diet Pepsi"), c("Coca-Cola",
                                                                                                                               "Other", "Pepsi ", "MeanDecreaseAccuracy", "MeanDecreaseGini"
                                                                            )))
zs <- structure(c(7.83991086892129, 21.0294946094815, 17.8567751826571,
                  -8.10621372424595, 20.4851609485216, 8.92315939431956, -1.76672951010352,
                  -0.902631046325599, -1.20617774719906, 1.26039812140835, 23.7632427837893,
                  16.1519992118888), .Dim = 3:4, .Dimnames = list(c("q5: feminine - Coke Zero", "q5: health-conscious - Coke Zero",
                                                                    "q5: innocent - Diet Pepsi"), c("Coca-Cola", "Other", "Pepsi ", "MeanDecreaseAccuracy"
                                                                    )))

ps <- structure(c(4.44089209850063e-15, 0, 0, 4.44089209850063e-16,
                  0, 0, 0.0772735387596579, 0.366721743515093, 0.227748962200478,
                  0.207525779019385, 0, 0), .Dim = 3:4, .Dimnames = list(c("q5: feminine - Coke Zero",
                                                                           "q5: health-conscious - Coke Zero", "q5: innocent - Diet Pepsi"), c("Coca-Cola", "Other", "Pepsi ", "MeanDecreaseAccuracy"
                                                                           )))

ft <- "Yo! This footer specifically designed
    to communicate important information.
    Since it is so important, it will of course
    extend over many lines.  In fact, on narrow tables,
    it might take >3.  On wide tables, it might only
    require one.  Feel free to adjust the width,
    and the importance and significance does not
    go away."

expect_error(RandomForestTable(importance, zs, ps, title = "Importance",
                               subtitle = "Big brown dog", footer = ft), NA)

importance <- structure(c(0.0103282370201527, 0.00496309436663631, 0.00699531041233543,
                          -0.00435907635563155, 4.91413943891512, 4.89903502762221, 4.95721719233815,
                          1.76659981232832), .Dim = c(4L, 2L), .Dimnames = list(c("q5: feminine - Coke Zero",
                                                                                  "q5: health-conscious - Coke Zero", "q5: innocent - Diet Pepsi", "Awareness: Diet Pepsi"), c("%IncMSE", "IncNodePurity")))

zs <- structure(c(12.172588531745, 5.73928091157484, 7.96220200183792,
                  -6.31415838610473), .Names = c("q5: feminine - Coke Zero", "q5: health-conscious - Coke Zero", "q5: innocent - Diet Pepsi", "Awareness: Diet Pepsi"
                  ))

ps <- structure(c(0, 9.50793954679341e-09, 1.77635683940025e-15, 2.71635602899778e-10
                ), .Names = c("q5: feminine - Coke Zero", "q5: health-conscious - Coke Zero", "q5: innocent - Diet Pepsi", "Awareness: Diet Pepsi"))

expect_error(RandomForestTable(importance, zs, ps, title = "Importance",
                               subtitle = "Big brown dog", footer = ft), NA)
