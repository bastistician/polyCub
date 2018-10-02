
# polyCub

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
implementations of these cubature methods and facilitate their
use in different projects.
For example, **polyCub** is now used for epidemic models in
[**surveillance**](https://CRAN.R-project.org/package=surveillance),
and for phylogeographic analyses in
[**rase**](https://CRAN.R-project.org/package=rase).

<!--
* [**surveillance**](https://CRAN.R-project.org/package=surveillance)
  uses **polyCub** to evaluate the likelihood of self-exciting
  spatio-temporal point process models for infectious disease spread.

* [**rase**](https://CRAN.R-project.org/package=rase) uses **polyCub** to
  integrate bivariate Gaussian densities for phylogeographic analyses.
-->


<!--

## Examples

#### General-purpose cubature rules

* `polyCub.midpoint()`: Two-dimensional midpoint rule

* `polyCub.SV()`: product Gauss cubature

#### Cubature rules for specific types of functions

* `polyCub.iso()`: Efficient adaptive cubature for *isotropic* functions via
  line `integrate()` along the polygon boundary

* `polyCub.exact.Gauss()` and `circleCub.Gauss()`:
  Quasi-exact methods specific to the integration of the
  *bivariate Gaussian density* over polygonal and circular domains, respectively

-->


## License

The **polyCub** package is free and open source software, licensed under the GPLv2.
