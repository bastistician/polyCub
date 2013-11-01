## Construct circles with increasing accuracy and of different spatial classes
disc1 <- discpoly(c(0,0), 5, npoly=4, class = "owin")
disc2 <- discpoly(c(0,0), 5, npoly=16, class = "Polygon")

## Look at the results
print(disc1)
plot(disc1, axes=TRUE, main="", border=2)

print(disc2)
lines(disc2, col=3)

if (require("rgeos")) { # for the "gpc.poly" class definition
    disc3 <- discpoly(c(0,0), 5, npoly=64, class = "gpc.poly")
    print(disc3)
    plot(disc3, add=TRUE, poly.args=list(border=4))
}

## if one only wants to _draw_ a circle without an object behind
symbols(0, 0, circles=5, inches=FALSE, add=TRUE, fg=5)
