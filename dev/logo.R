#!/usr/bin/Rscript
## Create the package logo at man/figures/logo.png
## Copyright (C) 2018-2019 Sebastian Meyer <seb.meyer@fau.de>
stopifnot(requireNamespace("polyCub")) # for plotpolyf()

PKGROOT <- if (basename(getwd()) == "dev") ".." else "."
stopifnot(file.exists(file.path(PKGROOT, "DESCRIPTION")))
OUTPATH <- file.path(PKGROOT, "man", "figures")
dir.create(OUTPATH, showWarnings = FALSE, recursive = TRUE)

## example function and polygon
f <- function (s, sigma = 5)
{
    exp(-rowSums(s^2)/2/sigma^2) / (2*pi*sigma^2)
}
hexagon <- list(
    list(x = c(7.33, 7.33, 3, -1.33, -1.33, 3),
         y = c(-0.5, 4.5, 7, 4.5, -0.5, -3))
)

## only plot inner contours
.f_inside_hexagon <- function (s, ...) {
    inside <- sp::point.in.polygon(s[,1], s[,2], hexagon[[1]]$x, hexagon[[1]]$y)
    ifelse(inside, f(s, ...), NA_real_)
}

## plot function
plotit <- function (scale = 1)
{
    par(bg = "transparent", ann = FALSE, bty = "n", xaxt = "n", yaxt = "n", mar = c(0,0,0,0))
    polyCub::plotpolyf(hexagon, .f_inside_hexagon, use.lattice = FALSE,
                       xlim = range(hexagon[[1]]$x), ylim = range(hexagon[[1]]$y))
    text(3, 2, labels = "\u222B", cex = 16*scale)  # Unicode integral symbol
}

## 'pkgdown' recommends 240 x 278 pixels (2.5 x 2.9 inch at 96 dpi)
png(file.path(OUTPATH, "logo.png"), width = 240, height = 278, res = 96)
plotit(scale = 0.5)
dev.off()
