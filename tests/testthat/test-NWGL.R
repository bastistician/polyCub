
context("Validation of cached Gauss-Legendre nodes/weights")

test_that("statmod::gauss.quad() still gives the same result", {
    new.NWGL <- lapply(seq_len(60L), function (n)
                       unname(statmod::gauss.quad(n = n, kind = "legendre")))
    expect_that(new.NWGL, equals(.NWGL, check.attributes = FALSE))
})
