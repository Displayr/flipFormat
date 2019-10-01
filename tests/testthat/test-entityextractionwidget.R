context("EntityExtractionWidget")

title <- paste0("Entity Extraction: What don't you like about Tom Cruise?  ",
                "That is, what is it about Tom Cruise that you dislike?")

entity.percentages <- c(Number = 0.02, Title = 0.08, Religion = 0.106666666666667,
                        Person = 0.0866666666666667, `Cause of death` = 0.00333333333333333,
                        Date = 0.01, Ideology = 0.00666666666666667, City = 0.00333333333333333)
variant.percentages <- list(Number = c(I = 0.00333333333333333, `11` = 0.00333333333333333,
                                       one = 0.00666666666666667, `90s` = 0.00333333333333333,
                                       `80s` = 0.00333333333333333, One = 0.00333333333333333),
                            Title = c(actor = 0.08, director = 0.00333333333333333),
                            Religion = c(scientology = 0.0266666666666667, Scientologist = 0.01,
                                         christian = 0.00333333333333333,  Scientology = 0.04,
                                         scientologist = 0.01,
                                         `Scientology cult` = 0.00333333333333333,
                                         cult = 0.00666666666666667,
                                         `christian scientist` = 0.00333333333333333,
                                         SCIENTOLOGY = 0.00333333333333333),
                            Person = c(`Nicole Kidman` = 0.00333333333333333,
                                       `Chelsea Clinton` = 0.00333333333333333, `Tom Cruise` = 0.05,
                                       Nicole = 0.00333333333333333, T.C. = 0.00333333333333333,
                                       `Tom cruise` = 0.00666666666666667,
                                       Brooke = 0.00333333333333333,
                                       `Katie Holmes` = 0.00666666666666667,
                                       Tom = 0.00333333333333333, Hfdthvi = 0.00333333333333333),
                            `Cause of death` = c(War = 0.00333333333333333),
                            Date = c(`10th year` = 0.00333333333333333,
                                     `the last several years` = 0.00333333333333333,
                                     `this day` = 0.00333333333333333),
                            Ideology = c(neutral = 0.00333333333333333,
                                         `church of scientology` = 0.00333333333333333),
                            City = c(Hollywood = 0.00333333333333333))
entity.counts = c(Number = 6L, Title = 24L, Religion = 32L, Person = 26L, `Cause of death` = 1L,
                  Date = 3L, Ideology = 2L, City = 1L)
variant.percentages = list( Number = c(I = 0.00333333333333333, `11` = 0.00333333333333333,
                                       one = 0.00666666666666667, `90s` = 0.00333333333333333,
                                       `80s` = 0.00333333333333333, One = 0.00333333333333333
                                       ),
                            Title = c(actor = 0.08, director = 0.00333333333333333),
                            Religion = c(scientology = 0.0266666666666667, Scientologist = 0.01,
                                         christian = 0.00333333333333333, Scientology = 0.04,
                                         scientologist = 0.01,
                                         `Scientology cult` = 0.00333333333333333,
                                         cult = 0.00666666666666667,
                                         `christian scientist` = 0.00333333333333333,
                                         SCIENTOLOGY = 0.00333333333333333),
                            Person = c(`Nicole Kidman` = 0.00333333333333333,
                                       `Chelsea Clinton` = 0.00333333333333333,
                                       `Tom Cruise` = 0.05, Nicole = 0.00333333333333333,
                                       T.C. = 0.00333333333333333,
                                       `Tom cruise` = 0.00666666666666667,
                                       Brooke = 0.00333333333333333,
                                       `Katie Holmes` = 0.00666666666666667,
                                       Tom = 0.00333333333333333,
                                       Hfdthvi = 0.00333333333333333),
                            `Cause of death` = c(War = 0.00333333333333333),
                            Date = c(`10th year` = 0.00333333333333333,
                                     `the last several years` = 0.00333333333333333,
                                     `this day` = 0.00333333333333333),
                            Ideology = c(neutral = 0.00333333333333333,
                                         `church of scientology` = 0.00333333333333333),
                            City = c(Hollywood = 0.00333333333333333))
variant.counts = list(Number = c(I = 1, `11` = 1, one = 2, `90s` = 1, `80s` = 1, One = 1),
                      Title = c(actor = 24, director = 1),
                      Religion = c(scientology = 8, Scientologist = 3, christian = 1,
                                   Scientology = 12, scientologist = 3, `Scientology cult` = 1,
                                   cult = 2, `christian scientist` = 1, SCIENTOLOGY = 1),
                      Person = c(`Nicole Kidman` = 1, `Chelsea Clinton` = 1, `Tom Cruise` = 15,
                                 Nicole = 1, T.C. = 1, `Tom cruise` = 2, Brooke = 1,
                                 `Katie Holmes` = 2, Tom = 1, Hfdthvi = 1),
                      `Cause of death` = c(War = 1),
                      Date = c(`10th year` = 1, `the last several years` = 1, `this day` = 1),
                      Ideology = c(neutral = 1, `church of scientology` = 1),
                      City = c(Hollywood = 1))
footer <- "Text was processed for Entity detection using 300 cases. There was 1 missing case."

test_that("Widget output check",
{
    result <- EntityExtractionWidget(entity.percentages, variant.percentages, entity.counts,
                                     variant.counts, title, footer)
    expect_is(result, "htmlwidget")
})

