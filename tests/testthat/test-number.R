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

