### Example application and comparison of the different cubature methods

## 2D-function to integrate (here: isotropic zero-mean Gaussian density)
f <- function (s, sigma = 5) exp(-rowSums(s^2)/2/sigma^2) / (2*pi*sigma^2)

## simple polygonal integration domain
library("spatstat")
disc.owin <- disc(radius=5, centre=c(3,2), npoly=8)

## plot image of the function and integration domain
gr <- seq(-8, 8, by=0.2)
vals <- matrix(f(expand.grid(gr,gr)), nrow=length(gr))
image(x=gr, y=gr, z=vals)
plot.owin(disc.owin, hatch=TRUE, add=TRUE)

## Two-dimensional midpoint rule
testmidpoint <- function (eps, main=paste("2D midpoint rule with eps =",eps))
{
    image(x=gr, y=gr, z=vals, main=main)
    plot.owin(disc.owin, hatch=TRUE, add=TRUE)
    ## add binary mask to plot
    #plot(as.mask(disc.owin, eps=eps), add=TRUE, box=FALSE)
    ## add evaluation points to plot
    with(as.mask(disc.owin, eps=eps),
         points(expand.grid(xcol, yrow), col=m, pch=20))
    polyCub.midpoint(disc.owin, f, eps=eps)
}
testmidpoint(5)
testmidpoint(3)
testmidpoint(0.5)
testmidpoint(0.2)

## Product Gauss cubature using an increasing number of nodes
for (nGQ in c(1:5,10,20,60)) {
    cat("nGQ =", sprintf("%2i",nGQ), ": ",
        format(polyCub.SV(disc.owin, f, nGQ=nGQ), digits=16),
        "\n")
}

## Quasi-exact cubature by triangulation and using mvtnorm::pmvnorm()
if (require("mvtnorm") && require("gpclib")) {
    gpclibPermit()
    print(polyCub.exact.Gauss(disc.owin, mean=c(0,0), Sigma=5^2*diag(2),
                              plot=TRUE), digits=16)
}
