context("MANOVA")
data(colas, package = "flipExampleData")
z <- unclass(colas$q4a)
#z[z == 4] <- 9
#z[z == 5] <- 4
#z[z == 9] <- 5
flipFormat::Labels(z) <- "Like Coca-Cola"
attr(z, "question") <- "Question"
colas$like.coke <- z - 3
attr(colas$q4b, "question") <- "Questsion"
colas$balanced <- c(rep(1:3, rep(100,3)), rep(NA, 27))
colas$agenumeric <- car::recode(colas$d1, as.factor.result = FALSE, recodes = "'18 to 24' = 21; '25 to 29' = 27; '30 to 34' = 32; '35 to 39' = 37; '40 to 44' = 42; '45 to 49' = 47; '50 to 54' = 52; '55 to 64' = 60; '65 or more' = 77")
colas$d1MISSING <- colas$d1
colas$like.cokeMISSING <-  colas$like.coke
set.seed(123)
colas$d1MISSING[runif(length(colas$d1MISSING)) > .75] <- NA
colas$like.cokeMISSING[runif(length(colas$d1MISSING)) > .75] <- NA

flipAnalysisOfVariance::OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = TRUE)



        expect_equal(z$title,"MANOVA: Age")
        expect_equal(names(z$anovas)[5], "Gender: Female")
        z <- OneWayMANOVA(data.frame(colas$q4b, colas$d3, colas$like.coke), colas$d1, binary = TRUE, show.labels = FALSE)
        expect_equal(z$title, "MANOVA: colas$d1")
        expect_equal(names(z$anovas)[4], "colas.q4b.5")
