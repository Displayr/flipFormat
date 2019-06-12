context("TurfTable")

output.table <- structure(c(90.8707865168539, 90.3089887640449, 89.747191011236,
            89.6067415730337, 88.623595505618, 88.623595505618, 88.3426966292135,
            87.9213483146067, 87.9213483146067, 87.5, 1055, 1175, 1181, 1205,
            1031, 1014, 1121, 1014, 971, 937), .Dim = c(10L, 2L), .Dimnames = list(
                c("Classic Bubble Gum, Sour, Strawberry", "Classic Bubble Gum, Grape, Strawberry",
                  "Classic Bubble Gum, Grape, Orange", "Classic Bubble Gum, Grape, Sour",
                  "Classic Bubble Gum, Orange, Strawberry", "Classic Bubble Gum, Strawberry, Cola",
                  "Classic Bubble Gum, Grape, Apple", "Grape, Sour, Strawberry",
                  "Classic Bubble Gum, Strawberry, Apple", "Classic Bubble Gum, Strawberry, Chocolate"
                ), c("Reach (%)", "Frequency")))

portfolios <- structure(c(1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 3, 3, 4, 3, 6, 3, 6,
                          4, 6, 5, 4, 6, 6, 8, 8, 5, 9, 6, 7, 6), .Dim = c(10L, 3L))

alternative.names <- c("Classic Bubble Gum", "Super-Strong Bubble Gum", "Grape", "Sour",
                       "Orange", "Strawberry", "Cola", "Apple", "Chocolate", "Peach",
                       "Watermelon")

TURFTable(output.table = output.table,
          portfolios = portfolios,
          alternative.names = alternative.names,
          title  = "Title here",
          subtitle = "Subtitle here",
          footer = "Footer here")
