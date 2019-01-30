---
title: '`polyCub`: An R package for Integration over Polygons'
authors:
 - name: Sebastian Meyer
   orcid: 0000-0002-1791-9449
   affiliation: 1
affiliations:
 - name: "Friedrich-Alexander-Universität Erlangen-Nürnberg (FAU),
     Erlangen, Germany"
   index: 1
date: "30 January 2019"
tags:
  - R
  - numerical integration
  - cubature
  - polygons
bibliography: paper.bib
---

# Summary

<!-- Multidimensional integrals are "not easy" -->
<!-- [@Press.etal2007, Section 4.8]. -->

The integral of a continuously differentiable function $f(x,y)$ over a
domain $W \subset \mathbb{R}^2$ can be approximated using an $n$-point
cubature rule of the form

$$ \iint_W f(x,y) \: \mathrm{d}x \, \mathrm{d}y
\approx \sum_{i=1}^n w_i f(x_i,y_i) \:, $$

i.e., a weighted sum of function values at an appropriate set of nodes.
In the special but common case of integration along the axes, i.e.,
$W = (x_l, x_u) \times (y_l, y_u)$, the domain is *rectangular*.
Several software packages implement numerical integration over such
rectangles (or *hypercubes* in higher dimensions), for example, the Cuba
[@hahn2005] or cubature [@johnson2017] libraries, which are both
interfaced from the `cubature` package [@R:cubature] in R [@R:base].

In spatial statistics, however, the domains of interest typically
correspond to geographic regions (administrative districts, lakes, etc.),
which are described by *polygons*. Solving integrals over such complex
domains requires specialized cubature methods, thus the R package
`polyCub`. A simple graphical summary of the purpose of `polyCub`
is given by its logo (see below).

![`polyCub`: Cubature over polygonal domains.](polyCub-logo.png)

<!-- As of version 0.7.0, -->
`polyCub` implements the following methods for
numerical integration over polygons:

* General-purpose *product Gauss cubature* [@sommariva.vianello2007]

* Simple *two-dimensional midpoint rule* via `spatstat` [@baddeley.turner2005]

* Adaptive cubature for *radially symmetric functions*
  $f(x,y) = f_r(\lVert(x-x_0,y-y_0)\rVert)$
  via integration along the polygon boundary
  [@meyer.held2014, Supplement B]

* Accurate (but slow) integration of the *bivariate Gaussian density*
  based on polygon triangulation
  [@Abramowitz.Stegun1972, Section 26.9, Example 9]


# Usage

The R package `polyCub` is released on the Comprehensive R Archive Network
([CRAN](https://CRAN.R-project.org/package=polyCub)) and can thus be
easily installed using `install.packages("polyCub")` in R.
After that, the basic usage is
```r
library("polyCub")
polyCub(polyregion, f)
```
where `polyregion` is the integration domain and `f` is the integrand.
Details are given in
```r
vignette("polyCub")
```
which exemplifies the implemented cubature methods by solving the integral
displayed in the package logo.

`polyCub` is currently used by at least two other R packages:
in `surveillance`, to evaluate the likelihood of self-exciting
spatio-temporal point process models for infectious disease spread
[@meyer.etal2017], and in `rase`, to integrate bivariate Gaussian densities
for phylogeographic analyses [@quintero.etal2015].


# References
