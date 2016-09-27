context("number")

test_that("Percents", {

    expect_equal(FormatAsPercent(0.00034), "0.034%")
    expect_equal(FormatAsPercent(0.000341546654), "0.034%")
    expect_equal(FormatAsPercent(.341546654), "34%")
    expect_equal(FormatAsPercent(3.41546654), "342%")
    expect_equal(FormatAsPercent(34.1546654), "3,415%")
    expect_equal(FormatAsPercent(34154.6654), "3,415,467%")
})

test_that("Numbers", {

    expect_equal(FormatAsReal(0.00034), "0.00034")
    expect_equal(FormatAsReal(0.000341546654), "0.00034")
    expect_equal(FormatAsReal(.341546654), "0.34")
    expect_equal(FormatAsReal(3.41546654), "3.4")
    expect_equal(FormatAsReal(3415.46654), "3,415")
    expect_equal(FormatAsReal(3415466.54), "3,415,467")
    expect_equal(FormatAsReal(NA), "NA")
})

test_that("PValue", {

    expect_equal(FormatAsPValue(0.0), "0.000000000000")
    expect_equal(FormatAsPValue(0.0000001), "0.0000001")
    expect_equal(FormatAsPValue(0.0000000000001), "0.000000000000")
    expect_equal(FormatAsPValue(0.04999), "0.04999")
    expect_equal(FormatAsPValue(0.050000), "0.050000000000")
    expect_equal(FormatAsPValue(0.05000000001), "0.05000000001")
    expect_equal(FormatAsPValue(0.059999999999), "0.06")
})


test_that("Vector number formats", {
    expect_equal(FormatAsPercent(c(0.00034, 0.000341546654)), c("0.034%", "0.034%"))
    expect_equal(FormatAsPercent(c(0.00034, NA, 0.000341546654)), c("0.034%", "NA", "0.034%"))
    expect_equal(FormatAsReal(c(0.00034, NA, 0.000341546654)), c("0.00034", "NA", "0.00034"))
    expect_equal(FormatAsPValue(c(0.00034, NA, 0.039999, 0.049999, 0.05001)), c("0.0003", "NA", "0.04", "0.049999", "0.05001"))
    expect_equal(FormatWithDecimals(c(0.00034, NA, 0.039999, 0.049999, 0.05001)), c("0.00", "NA", "0.04", "0.05", "0.05"))
})

test_that("Decimal places", {
    expect_equal(FormatWithDecimals(0.00034), "0.00")
    expect_equal(FormatWithDecimals(0.000341546654), "0.00")
    expect_equal(FormatWithDecimals(.341546654), "0.34")
    expect_equal(FormatWithDecimals(3.41546654), "3.42")
    expect_equal(FormatWithDecimals(3415.46654), "3,415.47")
    expect_equal(FormatWithDecimals(3415466.54), "3,415,466.54")
})

