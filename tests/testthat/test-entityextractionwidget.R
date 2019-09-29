context("EntityExtractionWidget")

title <- paste0("Entity Extraction: What don't you like about Tom Cruise?  ",
                "That is, what is it about Tom Cruise that you dislike?")

entity.percentages <- c(NUMBER = 0.02, TITLE = 0.08, RELIGION = 0.106666666666667,
                        PERSON = 0.0866666666666667, CAUSE_OF_DEATH = 0.00333333333333333,
                        DATE = 0.01, IDEOLOGY = 0.00666666666666667, CITY = 0.00333333333333333)
variant.percentages <- list(NUMBER = c(I = 0.00333333333333333, `11` = 0.00333333333333333,
                                       one = 0.00666666666666667, `90s` = 0.00333333333333333, `80s` = 0.00333333333333333,
                                       One = 0.00333333333333333),
                            TITLE = c(actor = 0.08, director = 0.00333333333333333),
                            RELIGION = c(scientology = 0.0266666666666667, Scientologist = 0.01,
                                                       christian = 0.00333333333333333, Scientology = 0.04, scientologist = 0.01,
                                                       `Scientology cult` = 0.00333333333333333, cult = 0.00666666666666667,
                                                       `christian scientist` = 0.00333333333333333, SCIENTOLOGY = 0.00333333333333333),
                            PERSON = c(`Nicole Kidman` = 0.00333333333333333, `Chelsea Clinton` = 0.00333333333333333,
                                                     `Tom Cruise` = 0.05, Nicole = 0.00333333333333333, T.C. = 0.00333333333333333,
                                                     `Tom cruise` = 0.00666666666666667, Brooke = 0.00333333333333333,
                                                     `Katie Holmes` = 0.00666666666666667, Tom = 0.00333333333333333,
                                                     Hfdthvi = 0.00333333333333333),
                            CAUSE_OF_DEATH = c(War = 0.00333333333333333),
                            DATE = c(`10th year` = 0.00333333333333333, `the last several years` = 0.00333333333333333,
                                     `this day` = 0.00333333333333333),
                            IDEOLOGY = c(neutral = 0.00333333333333333, `church of scientology` = 0.00333333333333333),
                            CITY = c(Hollywood = 0.00333333333333333))

test_that("Raw text diagnostics",
{
    result <- EntityExtractionWidget(entity.percentages, variant.percentages,
                                     title, "Footer here")
})

