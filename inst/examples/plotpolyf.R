### a polygonal domain
polycenter <- c(3,2)
mypoly <- discpoly(center=polycenter, radius=5, npoly=5)

### f: isotropic exponential decay
fr <- function(r, rate=1) dexp(r, rate=rate)
fcenter <- c(1,5)
f <- function (s, rate=1) fr(sqrt(rowSums(t(t(s)-fcenter)^2)), rate=rate)

### plot
plotpolyf(mypoly, f)
