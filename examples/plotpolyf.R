### a polygonal domain (a simplified version of spatstat.data::letterR$bdry)
letterR <- list(
    list(x = c(2.7, 3, 3.3, 3.9, 3.7, 3.4, 3.8, 3.7, 3.4, 2, 2, 2.7),
         y = c(1.7, 1.6, 0.7, 0.7, 1.3, 1.8, 2.2, 2.9, 3.3, 3.3, 0.7, 0.7)),
    list(x = c(2.6, 2.6, 3, 3.2, 3),
         y = c(2.2, 2.7, 2.7, 2.5, 2.2))
)

### f: isotropic exponential decay
fr <- function(r, rate = 1) dexp(r, rate = rate)
fcenter <- c(2,3)
f <- function (s, rate = 1) fr(sqrt(rowSums(t(t(s)-fcenter)^2)), rate = rate)

### plot
plotpolyf(letterR, f, use.lattice = FALSE)
plotpolyf(letterR, f, use.lattice = TRUE)
