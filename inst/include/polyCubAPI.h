/*******************************************************************************
 * Header file with wrapper functions for the C-routines provided by polyCub
 *
 * Copyright (C) 2017 Sebastian Meyer
 *
 * This file is part of the R package "polyCub",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at https://www.R-project.org/Licenses/.
 ******************************************************************************/

#include <stdlib.h>         // NULL
#include <Rinternals.h>     // SEXP
#include <R_ext/Rdynload.h> // R_GetCCallable

typedef double (*intrfr_fn) (double, double*);

void polyCub_iso(
    double *x, double *y,               // vertex coordinates (open)
    int *L,                             // number of vertices
    intrfr_fn intrfr,                   // F(R)
    double *pars,                       // parameters for F(R)
    double *center_x, double *center_y, // center of isotropy
    int *subdivisions, double *epsabs, double *epsrel, // Rdqags options
    int *stop_on_error,                 // !=0 means to stop at first ier > 0
    double *value, double *abserr, int *neval) // results
{
    static void(*fun)(double*,double*,int*,intrfr_fn,double*,double*,double*,
                      int*,double*,double*,int*,double*,double*,int*) = NULL;
    if (fun == NULL)
        fun = (void(*)(double*,double*,int*,intrfr_fn,double*,double*,double*,
                       int*,double*,double*,int*,double*,double*,int*))
            R_GetCCallable("polyCub", "polyiso");
    fun(x, y, L, intrfr, pars, center_x, center_y,
        subdivisions, epsabs, epsrel, stop_on_error,
        value, abserr, neval);
    return;
}
