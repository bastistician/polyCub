## a function to integrate (here: isotropic zero-mean Gaussian density)
f <- function (s, sigma = 5)
    exp(-rowSums(s^2)/2/sigma^2) / (2*pi*sigma^2)

## a simple polygon as integration domain
hexagon <- list(
    list(x = c(7.33, 7.33, 3, -1.33, -1.33, 3),
         y = c(-0.5, 4.5, 7, 4.5, -0.5, -3))
)

