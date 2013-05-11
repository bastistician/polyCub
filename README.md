polyCub
=======

An R package providing methods for cubature (numerical integration) over
polygonal domains. Note that for cubature over simple hypercubes, the packages
[`cubature`](http://CRAN.R-project.org/package=cubature)
and [`R2cuba`](http://CRAN.R-project.org/package=R2cuba)
might be more appropriate.

Currently, the following three (non-adaptive) cubature methods are implemented:
* Two-dimensional midpoint rule (a simple wrapper around
`spatstat::as.im.function`) 
* Product Gauss cubature as proposed by [Sommariva and Vianello (2007,
*Bit Numerical Mathematics*, **47** (2), 441-453)](http://dx.doi.org/10.1007/s10543-007-0131-2)
* Quasi-exact method specific to the integration of the bivariate Gaussian
density.

The function `polyCub` is the main entry point of the package. It is a
wrapper around the above specific cubature methods.

A Short History of the Package
---------------------------------------
For any spatio-temporal point process model, the likelihood contains an integral of the conditional intensity function over the observation region. If this is a polygon, analytical solutions are only available for trivial cases of the intensity function -- thus the need of a cubature method over polygonal domains.

My Master's Thesis (2010) on ["Spatio-Temporal Infectious Disease Epidemiology based on Point Processes"](http://epub.ub.uni-muenchen.de/11703/) is the origin of this package. Section 3.2 therein offers a more detailed description and benchmark experiment of the above cubature methods (among others).

The implementation then went into the [`surveillance`](http://CRAN.R-project.org/package=surveillance) package, where it is used to fit `twinstim` self-exciting two-component spatio-temporal point process models described in [Meyer et al (2012, *Biometrics*, **68** (2), 607-616)](http://dx.doi.org/10.1111/j.1541-0420.2011.01684.x).
In May 2013, I decided to move the cubature functionality into a stand-alone package, since it might be useful for other projects as well -- and here we are.
