context("ComparisonTable")


maxdiff.comparison <- structure(list(`In-sample accuracy` = c(0.694701986754967, 0.484105960264901,
                                                              0.42317880794702, 0.65364238410596), `Out-sample accuracy` = c(0.599337748344371,
                                                                                                                             0.447019867549669, 0.483443708609272, 0.586092715231788), BIC = c(6736.69476378719,
                                                                                                                                                                                               8999.72365651982, 8435.88806686651, NA), `Log-likelihood` = c(-3214.16585242447,
                                                                                                                                                                                                                                                             -4188.64355581298, -4192.24711185507, NA), Classes = c(1, 2,
                                                                                                                                                                                                                                                                                                                    1, NA), Algorithm = c("HB-Stan", "HB-Stan", "Latent Class", "Ensemble"
                                                                                                                                                                                                                                                                                                                    ), `Time taken (s)` = structure(c(3.58, 2.59, 0.32, NA), .Names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                        "elapsed", "elapsed", ""))), row.names = c("Model 1", "Model 2",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Model 3", "Ensemble"), class = "data.frame")
expect_error(ComparisonTable(maxdiff.comparison,
                             order.values = FALSE,
                             title = "HERE IS THE TITLE",
                             subtitle = "Here is the subtitle",
                             footer = "The footer is here"), NA)

choice.comparison <- structure(list(`In-sample accuracy` = c(0.934649122807018, 0.945614035087719,
                                                             0.94780701754386, 0.953947368421053), `Out-sample accuracy` = c(0.688157894736842,
                                                                                                                             0.672368421052632, 0.684210526315789, 0.7), BIC = c(2420.53715076809,
                                                                                                                                                                                 3255.78286375754, 4034.14137337232, NA), `Log-likelihood` = c(-901.379670242583,
                                                                                                                                                                                                                                               -1007.14353596949, -1084.46380000905, NA), Classes = c(1, 2,
                                                                                                                                                                                                                                                                                                      3, NA), Algorithm = c("HB-Stan", "HB-Stan", "HB-Stan", "Ensemble"
                                                                                                                                                                                                                                                                                                      ), `Time taken (s)` = structure(c(36.2860000000001, 120.53, 154.288,
                                                                                                                                                                                                                                                                                                                                        NA), .Names = c(NA, "elapsed", "elapsed", ""))), .Names = c("In-sample accuracy",
                                                                                                                                                                                                                                                                                                                                                                                                    "Out-sample accuracy", "BIC", "Log-likelihood", "Classes", "Algorithm",
                                                                                                                                                                                                                                                                                                                                                                                                    "Time taken (s)"), row.names = c("Model 1", "Model 2", "Model 3",
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Ensemble"), class = "data.frame")
expect_error(ComparisonTable(choice.comparison,
                             order.values = FALSE,
                             title = "HERE IS THE TITLE",
                             subtitle = "Here is the subtitle",
                             footer = "The footer is here"), NA)

ml.comparison.categorical <- structure(list(`Underlying model` = c("RandomForest", "SupportVectorMachine",
                                                                   "Regression"), `Model type` = list(NULL, NULL, "Linear"), `In-sample accuracy` = c(0.589,
                                                                                                                                                      0.582, 0.2555)), row.names = c("Model 1", "Model 2", "Model 3"
                                                                                                                                                      ), class = "data.frame")

expect_error(ComparisonTable(ml.comparison.categorical,
                             order.values = FALSE,
                             title = "HERE IS THE TITLE",
                             subtitle = "Here is the subtitle",
                             footer = "The footer is here"), NA)

ml.comparison.regression <- structure(list(`Underlying model` = c("RandomForest", "SupportVectorMachine",
                                                                  "Regression"), `Model type` = list(NULL, NULL, "Linear"), `In-sample RMSE` = c(12.6410304335426,
                                                                                                                                                 13.4343269136234, 13.3854382029181), `In-sample R^2` = c(0.129061678601267,
                                                                                                                                                                                                          0.0163190276347409, 0.0234654059045916)), row.names = c("Model 1",
                                                                                                                                                                                                                                                                  "Model 2", "Model 3"), class = "data.frame")
expect_error(ComparisonTable(ml.comparison.regression,
                             order.values = FALSE,
                             title = "HERE IS THE TITLE",
                             subtitle = "Here is the subtitle",
                             footer = "The footer is here"), NA)
