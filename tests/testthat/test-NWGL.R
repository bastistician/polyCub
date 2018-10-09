context("Validation of cached Gauss-Legendre nodes/weights")

test_that("statmod::gauss.quad() still gives the same result", {
    new.NWGL <- lapply(
        X = seq_len(61L),
        FUN = function (n)
        unname(statmod::gauss.quad(n = n, kind = "legendre"))
    )
    expect_equal(new.NWGL, .NWGL, check.attributes = FALSE)
})

test_that("polyCub.SV() can fetch nodes and weights from 'statmod'", {
    diamond <- list(list(x = c(1,2,1,0), y = c(1,2,3,2)))
    nw <- polyCub.SV(diamond, f = NULL, nGQ = 83)
    expect_type(nw, "list")
})
