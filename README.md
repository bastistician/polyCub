The R package polyCub ([CRAN](https://CRAN.R-project.org/package=polyCub))
=====================

An R package providing methods for **cubature** (numerical integration) **over
polygonal domains**. Note that for cubature over simple hypercubes, the packages
[`cubature`](https://CRAN.R-project.org/package=cubature)
and [`R2Cuba`](https://CRAN.R-project.org/package=R2Cuba)
might be more appropriate (cf.
[`CRAN Task View: Numerical Mathematics`](https://CRAN.R-project.org/view=NumericalMathematics)).

The function `polyCub()` is the main entry point of the package. It is a
wrapper around the following specific cubature methods.

#### General-purpose cubature rules:
* `polyCub.midpoint()`: Two-dimensional midpoint rule (a simple wrapper around
  [`spatstat`](https://CRAN.R-project.org/package=spatstat)'s `as.im.function()`)
* `polyCub.SV()`: Product Gauss cubature as proposed by
  [Sommariva and Vianello (2007, *BIT Numerical Mathematics*)](https://doi.org/10.1007/s10543-007-0131-2)

#### Cubature rules for specific types of functions:
* `polyCub.iso()`: Efficient adaptive cubature for *isotropic* functions via
  line `integrate()` along the polygon boundary, as described in Supplement B of
  [Meyer and Held (2014, *The Annals of Applied Statistics*)](https://doi.org/10.1214/14-AOAS743)
* `polyCub.exact.Gauss()` and `circleCub.Gauss()`:
  Quasi-exact methods specific to the integration of the
  *bivariate Gaussian density* over polygonal and circular domains, respectively


A Short History of the Package
------------------------------

For any spatio-temporal point process model, the likelihood contains an integral
of the conditional intensity function over the observation region. If this is a
polygon, analytical solutions are only available for trivial cases of the
intensity function - thus the need of a cubature method over polygonal domains.

My Master's Thesis (2010) on
["Spatio-Temporal Infectious Disease Epidemiology based on Point Processes"](http://epub.ub.uni-muenchen.de/11703/)
is the origin of this package. Section 3.2 therein offers a more detailed
description and benchmark experiment of some of the above cubature methods (and
others).

The implementation then went into the
[`surveillance`](https://CRAN.R-project.org/package=surveillance) package, where
it is used to fit `twinstim()`, self-exciting two-component spatio-temporal
point process models, described in
[Meyer et al (2012, *Biometrics*)](https://doi.org/10.1111/j.1541-0420.2011.01684.x).
In May 2013, I decided to move the cubature functionality into a stand-alone
package, since it might be useful for other projects as well.
Subsequently, I developed the isotropic cubature method `polyCub.iso()` to
efficiently estimate point process models with a power-law distance decay of
interaction
([Meyer and Held, 2014, *The Annals of Applied Statistics*](https://doi.org/10.1214/14-AOAS743)).
