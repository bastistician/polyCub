## quasi-exact integration based on gpclib::tristrip() and mvtnorm::pmvnorm()
if (requireNamespace("mvtnorm") && gpclibPermit()) {
    hexagon.gpc <- new("gpc.poly", pts = lapply(hexagon, c, list(hole = FALSE)))
    plotpolyf(hexagon.gpc, f, xlim = c(-8,8), ylim = c(-8,8))
    print(polyCub.exact.Gauss(hexagon.gpc, mean = c(0,0), Sigma = 5^2*diag(2),
                              plot = TRUE), digits = 16)
}
