## image of the function and integration domain
plotpolyf(hexagon, f)

## use a degree of nGQ = 3 and show the corresponding nodes
polyCub.SV(hexagon, f, nGQ = 3, plot = TRUE)

## extract nodes and weights
nw <- polyCub.SV(hexagon, f = NULL, nGQ = 3)[[1]]
nrow(nw$nodes)

## manually apply the cubature rule
sum(nw$weights * f(nw$nodes))

## use an increasing number of nodes
for (nGQ in c(1:5, 10, 20, 60))
    cat(sprintf("nGQ = %2i: %.16f\n", nGQ,
                polyCub.SV(hexagon, f, nGQ = nGQ)))

## polyCub.SV() is the default method used by the polyCub() wrapper
polyCub(hexagon, f, nGQ = 3)  # calls polyCub.SV()


### now using a simple *rectangular* integration domain

rectangle <- list(list(x = c(-1, 7, 7, -1), y = c(-3, -3, 7, 7)))
polyCub.SV(rectangle, f, plot = TRUE)

## effect of rotation given a very low nGQ
opar <- par(mfrow = c(1,3))
polyCub.SV(rectangle, f, nGQ = 4, rotation = FALSE, plot = TRUE)
           title(main = "without rotation (default)")
polyCub.SV(rectangle, f, nGQ = 4, rotation = TRUE,  plot = TRUE)
           title(main = "standard rotation")
polyCub.SV(rectangle, f, nGQ = 4,
           rotation = list(P = c(0,0), Q = c(2,-3)), plot = TRUE)
           title(main = "custom rotation")
par(opar)

## comparison with cubature::adaptIntegrate()
if (require("cubature")) {
    fc <- function (s, sigma = 5)
        exp(-sum(s^2)/2/sigma^2) / (2*pi*sigma^2)
    adaptIntegrate(f = fc, lowerLimit = c(-1, -3), upperLimit = c(7, 7))
}
