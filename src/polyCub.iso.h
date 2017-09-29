/*******************************************************************************
 * Header file of polyCub.iso.c
 *
 * Copyright (C) 2017 Sebastian Meyer
 *
 * This file is part of the R package "polyCub",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at https://www.R-project.org/Licenses/.
 ******************************************************************************/

typedef double (*intrfr_fn) (double, double*);

void polyiso(
    double *x, double *y,               // vertex coordinates (open)
    int *L,                             // number of vertices
    intrfr_fn intrfr,                   // F(R)
    double *pars,                       // parameters for F(R)
    double *center_x, double *center_y, // center of isotropy
    int *subdivisions, double *epsabs, double *epsrel, // Rdqags options
    int *stop_on_error,                 // !=0 means to stop at first ier > 0
    double *value, double *abserr, int *neval); // results
