context("CreateCustomTable")

test_that("checkImageTag",
{
    txt.sq <- "<img src='https://www.dropbox.com/s/ukinuuzg0tbojqj/ING%20Logo.png?dl=1'>"
    expect_error(res <- checkImageTag(txt.sq), NA)
    expect_equal(nchar(res), 84)

    txt.dq <- "<img src='https://www.dropbox.com/s/ukinuuzg0tbojqj/ING%20Logo.png?dl=1'>"
    expect_error(res <- checkImageTag(txt.dq), NA)
    expect_equal(nchar(res), 84)

    txt.empty <- "<img src=''>"
    expect_warning(checkImageTag(txt.empty), "Table content contains an image tag with an invalid link")

    txt.badlink <- "<img src='blah'>Some text"
    expect_warning(res <- checkImageTag(txt.badlink),
                   "Table content contains an image tag with an invalid link")
    expect_equal(res, "Some text")

    txt.withattr <- "<a href=https://us.coca-cola.com/products/coca-cola-zero-sugar/ target='_blank'>
         <img src=https://wiki.q-researchsoftware.com/images/c/cb/CokeZero.png width='45' height='100'></a>"
    expect_error(res <- checkImageTag(txt.withattr), NA)
    expect_equal(res, paste0("<div>", txt.withattr, "</div>"))
})
