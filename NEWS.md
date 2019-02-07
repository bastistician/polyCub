polyCub 0.7.1 (2019-02-07)
==========================

* Added a *getting started* `vignette("polyCub")`
  (suggested by @wrathematics in
  [openjournals/joss-reviews#1056](https://github.com/openjournals/joss-reviews/issues/1056)).

* fix minor compiler warning about missing `types` field in `R_CMethodDef`
  (@wrathematics, [#1](https://github.com/bastistician/polyCub/issues/1)).


polyCub 0.7.0 (2018-10-11)
==========================

* Package **polyCub** no longer attaches package
  [**sp**](https://CRAN.R-project.org/package=sp)
  (moved from "Depends" to "Imports").

* The R code of the examples is no longer installed by default.
  Use the `--example` flag of R CMD INSTALL to achieve that.

* The README now exemplifies the four different cubature rules.


polyCub 0.6.1 (2017-10-02)
==========================

* The exported C-function `polyCub_iso()` ...

    * did not handle its `stop_on_error` argument correctly
      (it would always stop on error).
    
    * now detects non-finite `intrfr` function values and gives an
      informative error message (rather than just reporting "abnormal
      termination of integration routine").

* Package **polyCub** no longer strictly depends on package
  [**spatstat**](https://CRAN.R-project.org/package=spatstat).
  It is only required for `polyCub.midpoint()` and for polygon input of
  class `"owin"`.


polyCub 0.6.0 (2017-05-24)
==========================

* Added full C-implementation of `polyCub.iso()`, which is exposed as
  `"polyCub_iso"` for use by other R packages (notably future versions of
  [**surveillance**](https://CRAN.R-project.org/package=surveillance))
  via `LinkingTo: polyCub` and `#include <polyCubAPI.h>`.
    
* Accommodate CRAN checks:
  add missing import from **graphics**,
  register native routines and disable symbol search


polyCub 0.5-2 (2015-02-25)
==========================

* `polyCub.midpoint()` works directly with input polygons of classes
  `"gpc.poly"` and `"SpatialPolygons"`, since package **polyCub** now
  registers corresponding `as.owin`-methods.
    
* `polyCub.exact.Gauss()` did not work if the `tristrip` of the
  transformed input polygon contained degenerate triangles (spotted by
  Ignacio Quintero).

* Line integration in `polyCub.iso()` could break due to division by zero
  if the `center` point was part of the polygon boundary.


polyCub 0.5-1 (2014-10-24)
==========================

* Nodes and weights for `polyCub.SV()` were only cached up to `nGQ=59`,
  not 60 as announced in version 0.5-0. Fixed that which also makes
  examples truly run without **statmod**.

* In `polyCub.SV()`, the new special setting `f=NULL` means to only
  compute nodes and weights.

* Internal changes to the `"gpc.poly"` converters to accommodate
  [**spatstat**](https://CRAN.R-project.org/package=spatstat) 1.39-0.


polyCub 0.5-0 (2014-05-07)
==========================

* `polyCub.SV()` gained an argument `engine` to choose among available
  implementations. The new and faster C-implementation is the default.
  There should not be any numerical differences in the result of the
  cubature.

* Package [**statmod**](https://CRAN.R-project.org/package=statmod) is no
  longer strictly required (imported). Nodes and weights for
  Gauss-Legendre quadrature in `polyCub.SV()` are now cached in the
  **polyCub** package up to `nGQ=60`. **statmod**`::gauss.quad` is only
  queried for a higher number of nodes.
  

polyCub 0.4-3 (2014-03-14)
==========================

* `polyCub.iso()` ...

    * could not handle additional arguments for `integrate()` given in the
      `control` list.
    
    * now applies the `control` arguments also to the numerical
      approximation of `intrfr`.
    
* The `checkintrfr()` function is exported and documented.

* Added a CITATION file.


polyCub 0.4-2 (2014-02-12)
==========================

* `plotpolyf()` ...
    
    * gained an additional argument `print.args`, an optional list of
      arguments passed to `print.trellis()` if `use.lattice=TRUE`.

    * passed a *data frame* of coordinates to `f` instead of a matrix as
      documented.


polyCub 0.4-1 (2013-12-05)
==========================

* This version solely fixes a missing NAMESPACE import to make
  package **polyCub** again compatible with older versions of
  [**spatstat**](https://CRAN.R-project.org/package=spatstat) (< 1.33-0).


polyCub 0.4-0 (2013-11-19)
==========================

INFRASTRUCTURE
--------------

* [**rgeos**](https://CRAN.R-project.org/package=rgeos) (and therefore the
  GEOS library) is no longer strictly required (moved from "Imports" to
  "Suggests").

* Added `coerce`-methods from `"Polygons"` (or `"SpatialPolygons"` or
  `"Polygon"`) to `"owin"` (`as(..., "owin")`).

* S4-style `coerce`-methods between `"gpc.poly"` and `"Polygons"`/`"owin"`
  have been removed from the package (since we no longer import the formal
  class `"gpc.poly"` from **gpclib** or **rgeos**). However, there are two
  new functions `gpc2owin` and `owin2gpc` similar to those dropped from
  [**spatstat**](https://CRAN.R-project.org/package=spatstat) since
  version 1.34-0.

* Moved `discpoly()` back to
  [**surveillance**](https://CRAN.R-project.org/package=surveillance)
  since it is only used there.

* The latter two changes cause
  [**surveillance**](https://CRAN.R-project.org/package=surveillance)
  version 1.6-0 to be incompatible with this new version of **polyCub**.
  Appropriate modifications have been made in the new version 1.7-0 of
  **surveillance**.

SPEED-UP `polyCub.SV()`
-----------------------

* thorough optimization of `polyCub.SV()`-related code resulted in about
  27% speed-up:

    * use `mapply()` instead of a `for`-loop

	* avoid `cbind()`

    * use `tcrossprod()`

    * less object copying

MINOR CHANGES
-------------

* `xylist()` is now exported. It simply extracts polygon coordinates from
  various spatial classes (with same unifying intention as `xy.coords()`).
      
* A `polyregion` of class `"SpatialPolygons"` of length more than 1 now
  works in `polyCub`-methods.
      
* Use aspect ratio of 1 in `plotpolyf()`.


polyCub 0.3-1 (2013-08-22)
==========================

* This version solely fixes a few typos and a technical note from `R CMD
  check` in the current R development version (also import packages into
  the NAMESPACE which are listed in the "Depends" field).


polyCub 0.3-0 (2013-07-06)
==========================

* New cubature method `polyCub.iso()` specific to isotropic functions
  (thanks to Emil Hedevang for the basic idea).

* New function `plotpolyf()` to plot a polygonal domain on top of an image
  of a bivariate function.

* The package now depends on R >= 2.15.0 (for `.rowSums()`).

* The package no longer registers `"owin"` as an S4-class since we depend
  on the **sp** package which does the job. This avoids a spurious warning
  (in `.simpleDuplicateClass()`) upon package installation.

* In `discpoly()`, the argument `r` has been renamed to `radius`. This is
  backward compatible by partial argument matching in old code.


polyCub 0.2-0 (2013-05-09)
==========================

* This is the initial version of the **polyCub** package mainly built on
  functions previously maintained within the
  [**surveillance**](https://CRAN.R-project.org/package=surveillance)
  package. These methods for cubature of polygonal domains have been
  outsourced into this separate **polyCub** package since they are of
  general use for other packages as well.
    
* The **polyCub** package has more documentation and tests, avoids the use
  of [**gpclib**](https://CRAN.R-project.org/package=gpclib) as far as
  possible (using [**rgeos**](https://CRAN.R-project.org/package=rgeos)
  instead), and solves a compatibility issue with package
  [**maptools**](https://CRAN.R-project.org/package=maptools) (use
  `setClass("owin")` instead of `setOldClass("owin")`).
