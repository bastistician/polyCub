/*******************************************************************************
 * C-version of polyCub1.iso()
 *
 * Copyright (C) 2015,2017 Sebastian Meyer
 *
 * This file is part of the R package "polyCub",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at https://www.R-project.org/Licenses/.
 ******************************************************************************/

/* The corresponding math is derived in Supplement B (Section 2.4) of
 * Meyer and Held (2014): "Power-law models for infectious disease spread."
 * The Annals of Applied Statistics, 8(3), 1612-1639.
 * https://doi.org/10.1214/14-AOAS743SUPPB
 */

#include <R_ext/Arith.h>   // R_FINITE, otherwise math.h would suffice
#include <R_ext/Error.h>   // error
#include <R_ext/Memory.h>  // R_alloc
#include <R_ext/Print.h>   // Rprintf
#include <R_ext/Applic.h>  // Rdqags

// header file defines the intrfr_fn type
#include "polyCub.iso.h"

// integrand for the edge (x0,y0) -> (x1,y1), see Equation 7
static double lineIntegrand(
    double t,
    double x0, double y0, double x1, double y1,
    intrfr_fn intrfr, double *pars)
{
    double num = y1*x0 - x1*y0;  // numerator term
    // point on the edge corresponding to t
    double px = x0 + t*(x1-x0);
    double py = y0 + t*(y1-y0);
    double norm2 = px*px + py*py;
    // evaluate F(R) = int_0^R r*f(r) dr at R=||(px,py)||
    double inti = intrfr(sqrt(norm2), pars);
    if (!R_FINITE(inti))
        error("non-finite intrfr value at R=%f", sqrt(norm2));
    return num*inti/norm2;
}

// set of parameters for line integration (passed via the *ex argument)
typedef struct {
    double x0, y0, x1, y1;
    intrfr_fn intrfr;
    double *pars;
} Params;

// vectorized lineIntegrand for use with Rdqags
static void myintegr_fn(double *x, int n, void *ex)
{
    Params *param = (Params *) ex;
    for(int i = 0; i < n; i++) {
        x[i] = lineIntegrand(x[i],
                             param->x0, param->y0, param->x1, param->y1,
                             param->intrfr, param->pars);
    }
    return;
}

// calculate line integral for one edge (x0,y0) -> (x1,y1)
// using Gauss-Kronrod quadrature via Rdqags as declared in <R_ext/Applic.h>,
// implemented in R/src/appl/integrate.c,
// and used in R/src/library/stats/src/integrate.c
static void polyiso_side(
    double x0, double y0, double x1, double y1,           // 2 vertices
    intrfr_fn intrfr, double *pars,                       // F(R)
    int subdivisions, double *epsabs, double *epsrel,     // control
    double *result, double *abserr, int *neval, int *ier) // results
{
    double num = y1*x0 - x1*y0;  // numerator in lineIntegrand
    // for any point p on the edge
    if (num == 0.0) { // 'center' is part of this polygon edge
        *result = 0.0;
        *abserr = 0.0;
        //*last = 0;
        *neval = 0;
        *ier = 0;
        return;
    }
    // set of parameters for lineIntegrand
    Params param = {x0, y0, x1, y1, intrfr, pars};
    // prepare for Rdqags
    double lower = 0.0;
    double upper = 1.0;
    int lenw = 4 * subdivisions;
    int last; // unused
    int *iwork = (int *) R_alloc((size_t) subdivisions, sizeof(int));
    double *work = (double *) R_alloc((size_t) lenw, sizeof(double));

    Rdqags(myintegr_fn, &param, &lower, &upper,
           epsabs, epsrel,
           result, abserr, neval, ier, // results
           &subdivisions, &lenw, &last, iwork, work);

    return;
}

// line integration along the edges of a polygon
void polyiso(
    double *x, double *y,               // vertex coordinates (open)
    int *L,                             // number of vertices
    intrfr_fn intrfr,                   // F(R)
    double *pars,                       // parameters for F(R)
    double *center_x, double *center_y, // center of isotropy
    int *subdivisions, double *epsabs, double *epsrel, // Rdqags options
    int *stop_on_error,                 // !=0 means to stop at first ier > 0
    double *value, double *abserr, int *neval) // results
{
    // auxiliary variables
    double resulti, abserri;
    int nevali, ieri;
    double x0, y0, x1, y1;
    int idxTo;
    // initialize result at 0 (do += for each polygon edge);
    *value = 0.0;
    *abserr = 0.0;
    *neval = 0;
    for (int i = 0; i < *L; i++) {
        x0 = x[i] - *center_x;
        y0 = y[i] - *center_y;
        idxTo = (i == *L-1) ? 0 : i+1;
        x1 = x[idxTo] - *center_x;
        y1 = y[idxTo] - *center_y;
        polyiso_side(x0, y0, x1, y1,
                     intrfr, pars,
                     *subdivisions, epsabs, epsrel,
                     &resulti, &abserri, &nevali, &ieri);
        if (ieri > 0) {
            if (*stop_on_error == 0) {
                Rprintf("abnormal termination of integration routine (%i)\n", ieri);
            } else {
                error("abnormal termination of integration routine (%i)\n", ieri);
            }
        }
        *value += resulti;
        *abserr += abserri;
        *neval += nevali;
    }
    return;
}
