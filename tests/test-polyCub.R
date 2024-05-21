if (!requireNamespace("spatstat.geom"))
    q("no")
library("polyCub")

## bivariate, isotropic Gaussian density
f <- function (s, mean, sd)
    dnorm(s[,1], mean=mean[1], sd=sd) * dnorm(s[,2], mean=mean[2], sd=sd)

## circular domain represented by a polygon
r <- 5
center <- c(3,2)
npoly <- 128
disc.owin <- spatstat.geom::disc(radius=r, centre=center, npoly=npoly)

## parameters for f
m <- c(1,1)
sd <- 3

## target value of the integral over the _polygonal_ circle
intExact <- 0.65844436  # taken from exact.Gauss cubature (below)
stopIfDiff <- function(int, ...)
    if(!isTRUE(all.equal.numeric(intExact, int, ..., check.attributes = FALSE))) {
        if (is.call(cl <- substitute(int))) cl <- cl[1]
        stop(deparse(cl), " result not equal to reference value")
    }

## reproduce saved reference value
if (identical(Sys.getenv("R_GPCLIBPERMIT"), "true") &&
    local({pkg <- "gpclib"; requireNamespace(pkg)}) && # undeclared ...
    requireNamespace("mvtnorm"))
    stopIfDiff(polyCub.exact.Gauss(disc.owin, mean=m, Sigma=sd^2*diag(2)),
               tolerance = 1e-8)

## exact value of the integral over the _real_ circle
stopIfDiff(circleCub.Gauss(center=center, r=r, mean=m, sd=sd),
           tolerance = 0.001)  # agreement depends on 'npoly'

## polyCub.midpoint
stopIfDiff(polyCub.midpoint(disc.owin, f, mean=m, sd=sd, dimyx=500),
           tolerance = 0.001)

## polyCub.SV
intC <- polyCub.SV(disc.owin, f, mean=m, sd=sd, nGQ=3, engine="C")
intR <- polyCub.SV(disc.owin, f, mean=m, sd=sd, nGQ=3, engine="R")
stopifnot(all.equal(intC, intR))
stopIfDiff(intC, tolerance = 0.0001)

## polyCub.iso (using a numerical approximation of intrfr)
stopIfDiff(polyCub.iso(disc.owin, f, mean=m, sd=sd, center=m))
