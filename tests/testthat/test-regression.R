context("Regression tests")

octagon <- spatstat::disc(radius = 5, centre = c(3,2), npoly = 8)
f <- function (s) (rowSums(s^2)+1)^-2
##plotpolyf(octagon, f)

test_that("isotropic cubature can handle control list for integrate()", {
    ## previosly, passing control arguments did not work
    int1 <- polyCub.iso(octagon, f, center=c(0,0), control=list(rel.tol=1e-3))
    int2 <- polyCub.iso(octagon, f, center=c(0,0), control=list(rel.tol=1e-8))
    ## results are almost but not identical
    expect_equal(int1, int2, tolerance=1e-3)
    expect_false(identical(int1, int2))
})
