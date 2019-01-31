if (require("spatstat")) {
    hexagon.owin <- owin(poly = hexagon)

    show_midpoint <- function (eps)
    {
        plotpolyf(hexagon.owin, f, xlim = c(-8,8), ylim = c(-8,8),
                  use.lattice = FALSE)
        ## add evaluation points to plot
        with(as.mask(hexagon.owin, eps = eps),
             points(expand.grid(xcol, yrow), col = t(m), pch = 20))
        title(main = paste("2D midpoint rule with eps =", eps))
    }

    ## show nodes (eps = 0.5)
    show_midpoint(0.5)

    ## show pixel image (eps = 0.5)
    polyCub.midpoint(hexagon.owin, f, eps = 0.5, plot = TRUE)

    ## use a decreasing pixel size (increasing number of nodes)
    for (eps in c(5, 3, 1, 0.5, 0.3, 0.1))
        cat(sprintf("eps = %.1f: %.7f\n", eps,
                    polyCub.midpoint(hexagon.owin, f, eps = eps)))
}
