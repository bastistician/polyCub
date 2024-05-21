
# polyCub <img src="man/figures/logo.png" align="right" alt="" width="120" />

The [R](https://www.R-project.org/) package **polyCub** implements
*cubature* (numerical integration) over *polygonal* domains.
It solves the problem of integrating a continuously differentiable
function f(x,y) over simple closed polygons.

For the special case of a rectangular domain along the axes, the
[**cubature**](https://CRAN.R-project.org/package=cubature)
package is more appropriate (cf.
[`CRAN Task View: Numerical Mathematics`](https://CRAN.R-project.org/view=NumericalMathematics)).


## Installation

You can install
[polyCub from CRAN](https://CRAN.R-project.org/package=polyCub)
via:

```R
install.packages("polyCub")
```

To install the development version from the
[GitHub repository](https://github.com/bastistician/polyCub), use:

```R
## install.packages("remotes")
remotes::install_github("bastistician/polyCub")
```


## Usage

The basic usage is:

```r
library("polyCub")
polyCub(polyregion, f)
```

* `polyregion` represents the integration domain as an object of class
`"owin"` (from **spatstat.geom**), `"gpc.poly"` (from **gpclib**),
`"SpatialPolygons"` (from **sp**), or `"(MULTI)POLYGON"` (from **sf**),
or even as a plain list of lists of vertex coordinates (`"xylist"`).

* `f` is the integrand and needs to take a two-column coordinate matrix
as its first argument.

The `polyCub()` function wraps the implemented cubature methods and
by default calls `polyCub.SV()`, a C-implementation of *product Gauss cubature*.
Directly calling the desired cubature function is preferable,
see the list below.

### Implemented cubature methods

1. `polyCub.SV()`:
   General-purpose **product Gauss cubature**
   (Sommariva and Vianello, 2007, *BIT Numerical Mathematics*,
   <https://doi.org/10.1007/s10543-007-0131-2>)

2. `polyCub.midpoint()`:
   Simple **two-dimensional midpoint rule** based on
   [**spatstat.geom**](https://CRAN.R-project.org/package=spatstat.geom)`::as.im.function()`

3. `polyCub.iso()`:
   Adaptive **cubature for radially symmetric functions**
   via line `integrate()` along the polygon boundary
   (Meyer and Held, 2014, *The Annals of Applied Statistics*,
   <https://doi.org/10.1214/14-AOAS743>, Supplement B, Section 2.4)

<!--
4. `polyCub.exact.Gauss()`:
   Accurate (but slow) **integration of the bivariate Gaussian density**
   based on polygon triangulation and
   [**mvtnorm**](https://CRAN.R-project.org/package=mvtnorm)`::pmvnorm()`
-->

For details and illustrations see the `vignette("polyCub")`
in the installed package or
[on CRAN](https://CRAN.R-project.org/package=polyCub/vignettes/polyCub.html).


## Applications

The **polyCub** package evolved from the need to integrate
so-called spatial interaction functions (Gaussian or power-law kernels)
over the observation region of a spatio-temporal point process.
Such epidemic models are implemented in
[**surveillance**](https://CRAN.R-project.org/package=surveillance).


## Feedback

Contributions are welcome!
Please submit suggestions or report bugs at
<https://github.com/bastistician/polyCub/issues>
or via e-mail to `maintainer("polyCub")`.


## License

The **polyCub** package is free and open source software, licensed under the GPLv2.
