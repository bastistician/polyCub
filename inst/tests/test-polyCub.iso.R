context("isotropic cubature method")

## a polygonal domain
polycenter <- c(3,2)
disc.owin <- spatstat::disc(radius=5, centre=polycenter, npoly=5)

test_that("isotropic cubature for exponential decay is correct", {
    fr <- function(r, rate=1) dexp(r, rate=rate)
    fcenter <- c(1,5)
    f <- function (s, rate=1) fr(sqrt(rowSums(t(t(s)-fcenter)^2)), rate=rate)

    ## numerical intrfr
    intMP <- polyCub.midpoint(disc.owin, f, dimyx=500)
    intISOdouble <- polyCub.iso(disc.owin, f, center=fcenter)
    expect_that(intMP,
                equals(intISOdouble, tolerance=0.001, check.attributes=FALSE))

    ## analytical intrfr
    intrfr <- function (x, rate=1) pgamma(x, 2, rate) / rate
    intISO <- polyCub.iso(disc.owin, f, intrfr, center=fcenter,
                          check.intrfr=TRUE)
    expect_that(intISOdouble, equals(intISO, check.attributes=FALSE))
})

test_that("isotropic cubature of constant function matches polygon area", {
    f.const <- function (s) 1
    intrfr.const <- function (x) x^2/2
    area.ISO <- polyCub.iso(disc.owin, intrfr=intrfr.const, center=polycenter)
    expect_that(area.ISO, equals(spatstat::area.owin(disc.owin),
                                 check.attributes=FALSE))
})
