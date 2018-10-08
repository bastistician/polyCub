




# polyCub <img src="https://raw.githubusercontent.com/bastistician/polyCub/master/figures/logo-1.png" align="right" />

The R package **polyCub** implements *cubature* over *polygonal* domains.
The goal is to approximate the integral of a continuously differentiable
function f(x,y) over a simple closed polygonal domain.

For the special case where the domain is rectangular with sides parallel
to the axes (such as a bounding box), the packages
[**cubature**](https://CRAN.R-project.org/package=cubature)
and [**R2Cuba**](https://CRAN.R-project.org/package=R2Cuba)
are more appropriate (cf.
[`CRAN Task View: Numerical Mathematics`](https://CRAN.R-project.org/view=NumericalMathematics)).


## Installation

You can install [polyCub from CRAN](https://CRAN.R-project.org/package=polyCub) via:

```R
install.packages("polyCub")
```

To install the development version from the GitHub repository, use:

```R
## install.packages("remotes")
remotes::install_github("bastistician/polyCub")
```


## Motivation

The **polyCub** package evolved from the need to evaluate integrals of
so-called spatial interaction functions (e.g., a Gaussian or power-law
kernel) over the observation region of a spatio-temporal point process
(Meyer et al, 2012, *Biometrics*, <https://doi.org/10.1111/j.1541-0420.2011.01684.x>).
Such an observation region is described by a polygonal boundary,
representing, for example, the shape of a country or administrative
district.

The integration task could be simplified by either assuming a trivial
kernel, such as f(x,y)=1, or by simply replacing the polygonal with a
rectangular domain, such as the bounding box of the polygon.
However, these crude approximations can be avoided by using efficient
numerical integration methods for polygonal domains:

* A starting point is the simple *two-dimensional midpoint rule* via
  `as.im.function()` from the
  [**spatstat**](https://CRAN.R-project.org/package=spatstat) package.

* Sommariva and Vianello (2007, *BIT Numerical Mathematics*,
  <https://doi.org/10.1007/s10543-007-0131-2>) proposed so-called *product
  Gauss cubature* and provided a reference implementation in Matlab.
  
* For *bivariate Gaussian densities*, integrals over polygons can be
  solved accurately using combinations of standard evaluations of Gaussian
  cumulative density functions (Abramowitz and Stegun, 1972, Section 26.9,
  Example 9).

* For *radially symmetric functions* f(x,y) = f_r(||(x-x_0,y-y_0)||),
  numerical integration can be made much more efficient via line
  `integrate()` along the boundary of the polygonal domain
  (Meyer and Held, 2014, *The Annals of Applied Statistics*,
  <https://doi.org/10.1214/14-AOAS743>, Supplement B, Section 2.4).

The dedicated R package **polyCub** was born in 2013 to provide
implementations of the above cubature methods and facilitate their
use in different projects.
For example, **polyCub** powers epidemic models in
[**surveillance**](https://CRAN.R-project.org/package=surveillance)
and phylogeographic analyses in
[**rase**](https://CRAN.R-project.org/package=rase).


## Example


```r
library("polyCub")
```

We consider a function f(x,y) which all of the above
cubature methods can handle: an isotropic zero-mean Gaussian density.
**polyCub** expects the function's implementation `f` to take a two-column
coordinate matrix as its first argument (as opposed to separate arguments
for the x and y coordinates):


```r
f <- function (s, sigma = 5)
{
    exp(-rowSums(s^2)/2/sigma^2) / (2*pi*sigma^2)
}
```

We use a simple hexagon as polygonal integration domain,
here specified via an `"xylist"` of vertex coordinates:


```r
hexagon <- list(
    list(x = c(7.33, 7.33, 3, -1.33, -1.33, 3),
         y = c(-0.5, 4.5, 7, 4.5, -0.5, -3))
)
```

An image of the function and the integration domain can be produced using
**polyCub**'s rudimentary (but convenient) plotting utility:


```r
plotpolyf(hexagon, f, xlim = c(-8,8), ylim = c(-8,8))
```

![Example](https://raw.githubusercontent.com/bastistician/polyCub/master/figures/example-1.png)

#### Supported polygon representations

The integration domain is typically represented using a dedicated class
for polygons, such as `"owin"` from package **spatstat**:


```r
library("spatstat")
hexagon.owin <- owin(poly = hexagon)
```

All of **polyCub**'s cubature methods as well as `plotpolyf()` understand

* `"owin"` from **spatstat**,

* `"gpc.poly"` from package **rgeos** (or **gpclib**), and

* `"SpatialPolygons"` from package **sp**.

Note that the integration domain may consist of more than one polygon
(including holes).


### `polyCub.SV()`: Product Gauss cubature

The **polyCub** package provides an R-interfaced C-translation of
"polygauss: Matlab code for Gauss-like cubature over polygons"
(Sommariva and Vianello, 2013, <http://www.math.unipd.it/~alvise/software.html>).
The cubature rule is based on Green's theorem and incorporates
appropriately transformed weights and nodes of one-dimensional
Gauss-Legendre quadrature in both dimensions,
thus the name "product Gauss cubature".
It is exact for all bivariate polynomials if the number of cubature nodes
is sufficiently large (depending on the degree of the polynomial).

For the above example, a reasonable approximation is already obtained
with degree `nGQ = 3` of the one-dimensional Gauss-Legendre quadrature:


```r
polyCub.SV(hexagon, f, nGQ = 3, plot = TRUE)
#> [1] 0.2741456
```

![Product Gauss cubature](https://raw.githubusercontent.com/bastistician/polyCub/master/figures/product-Gauss-1.png)

The involved nodes (displayed in the figure above) and weights can be
extracted by calling `polyCub.SV()` with `f = NULL`, e.g., to determine
the number of nodes:

```r
nrow(polyCub.SV(hexagon, f = NULL, nGQ = 3)[[1]]$nodes)
#> [1] 72
```

For illustration, we create a variant of `polyCub.SV()`,
which returns the number of function evaluations as an attribute:


```r
polyCub.SVn <- function (polyregion, f, ..., nGQ = 20) {
    nw <- polyCub.SV(polyregion, f = NULL, ..., nGQ = nGQ)
    ## nw is a list with one element per polygon of 'polyregion'
    res <- sapply(nw, function (x)
        c(result = sum(x$weights * f(x$nodes, ...)), nEval = nrow(x$nodes)))
    structure(sum(res["result",]), nEval = sum(res["nEval",]))
}
polyCub.SVn(hexagon, f, nGQ = 3)
#> [1] 0.2741456
#> attr(,"nEval")
#> [1] 72
```

We can use this function to investigate how the accuracy of the
approximation depends on the degree `nGQ` and the associated number of
cubature nodes:


```r
for (nGQ in c(1:5, 10, 20)) {
    result <- polyCub.SVn(hexagon, f, nGQ = nGQ)
    cat(sprintf("nGQ = %2i: %.12f (n=%i)\n", nGQ, result, attr(result, "nEval")))
}
#> nGQ =  1: 0.285265369245 (n=12)
#> nGQ =  2: 0.274027610314 (n=36)
#> nGQ =  3: 0.274145638288 (n=72)
#> nGQ =  4: 0.274144768964 (n=120)
#> nGQ =  5: 0.274144773834 (n=180)
#> nGQ = 10: 0.274144773813 (n=660)
#> nGQ = 20: 0.274144773813 (n=2520)
```


### `polyCub.midpoint()`: Two-dimensional midpoint rule

The two-dimensional midpoint rule in **polyCub** is a simple wrapper
around `as.im.function()` and `integral.im()` from package **spatstat**.

Using a pixel size of `eps = 0.5` (here yielding 270 pixels), we obtain:


```r
polyCub.midpoint(hexagon.owin, f, eps = 0.5, plot = TRUE)
#> [1] 0.2736067
```

![Midpoint rule](https://raw.githubusercontent.com/bastistician/polyCub/master/figures/midpoint-1.png)


### `polyCub.iso()`: Adaptive cubature for *isotropic* functions

A radially symmetric function f(x,y) = f(r) can be expressed in terms of
the distance r from its point of symmetry.
If the antiderivative of r times f(r), called `intrfr()`, is
analytically available, Green's theorem leads to a cubature rule
which only needs one-dimensional numerical integration.
More specifically, `intrfr()` will be `integrate()`d along the edges of
the polygon. The mathematical details are given in Meyer and Held (2014,
<https://doi.org/10.1214/14-AOAS743SUPPB>, Section 2.4).

For the bivariate Gaussian density `f` defined above,
the integral from 0 to R of `r*f(r)` is analytically available as:

```r
intrfr <- function (R, sigma = 5)
{
    (1 - exp(-R^2/2/sigma^2))/2/pi
}
```

With this information, we can apply the cubature rule as follows:


```r
polyCub.iso(hexagon, intrfr = intrfr, center = c(0,0))
#> [1] 0.2741448
#> attr(,"abs.error")
#> [1] 3.043618e-15
```

Note that we do not even need the original function `f`.

If `intrfr()` is missing, it can be approximated numerically using
`integrate()` for `r*f(r)` as well, but the overall integration will then
be much less efficient than product Gauss cubature.

Package **polyCub** exposes the backbone of `polyCub.iso()` as C function
`polyCub_iso()` for use by other R packages (notably **surveillance**) via
`LinkingTo: polyCub` and `#include <polyCubAPI.h>`. See
<https://github.com/bastistician/polyCub/blob/master/tests/testthat/polyiso_powerlaw.c>
for an example.


### `polyCub.exact.Gauss()`: Integration of the *bivariate Gaussian density*

Note: There is also a function `circleCub.Gauss()` to calculate the
integral of the bivariate Gaussian density over a *circular* domain.


## License

The **polyCub** package is free and open source software, licensed under the GPLv2.
