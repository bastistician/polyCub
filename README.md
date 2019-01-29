
# polyCub <img src="man/figures/logo.png" align="right" alt="" width="120" />

The R package **polyCub** implements *cubature* (numerical integration)
over *polygonal* domains.
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

To install the development version from the GitHub repository, use:

```R
## install.packages("remotes")
remotes::install_github("bastistician/polyCub")
```


## Cubature methods

1. General-purpose **product Gauss cubature** (Sommariva and Vianello, 2007,
   *BIT Numerical Mathematics*, <https://doi.org/10.1007/s10543-007-0131-2>)

2. Simple **two-dimensional midpoint rule**
   (via [**spatstat**](https://CRAN.R-project.org/package=spatstat))
  
3. **Adaptive cubature for radially symmetric functions**
   f(x,y) = f_r(||(x-x_0,y-y_0)||)
   via line `integrate()` along the polygon boundary
   (Meyer and Held, 2014, *The Annals of Applied Statistics*,
   <https://doi.org/10.1214/14-AOAS743>, Supplement B, Section 2.4)

4. Accurate (but slow) **integration of the bivariate Gaussian density**
   based on polygon triangulation and
   [**mvtnorm**](https://CRAN.R-project.org/package=mvtnorm)`::pmvnorm()`


## Details and illustrations

See the `vignette("polyCub")` in the installed package or
[on CRAN](https://CRAN.R-project.org/web/packages/polyCub/vignettes/polyCub.html).


## Use cases

The **polyCub** package evolved from the need to integrate
so-called spatial interaction functions (Gaussian or power-law kernels)
over the observation region (an administrative shapefile)
of a spatio-temporal point process.
Such epidemic models are implemented in
[**surveillance**](https://CRAN.R-project.org/package=surveillance).

**polyCub** also powers phylogeographic analyses in
[**rase**](https://CRAN.R-project.org/package=rase).


## Feedback

Contributions are welcome!
Please submit suggestions or report bugs at
<https://github.com/bastistician/polyCub/issues>
or via e-mail to `maintainer("polyCub")`.
Note that pull requests should only be submitted
after discussion of the underlying issue.


## License

The **polyCub** package is free and open source software, licensed under the GPLv2.
