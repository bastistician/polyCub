/*******************************************************************************
 * Example of using the C-routine "polyCub_iso", see also test-polyiso.R
 *
 * Copyright (C) 2015,2017 Sebastian Meyer
 *
 * This file is part of the R package "polyCub",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at https://www.R-project.org/Licenses/.
 ******************************************************************************/

#include <math.h>
#include <polyCubAPI.h>

// F(R) example
static double intrfr_powerlaw(double R, double *logpars)
{
        double sigma = exp(logpars[0]);
        double d = exp(logpars[1]);
        if (d == 1.0) {
                return R - sigma * log(R/sigma + 1);
        } else if (d == 2.0) {
                return log(R/sigma + 1) - R/(R+sigma);
        } else {
                return (R*pow(R+sigma,1-d) - (pow(R+sigma,2-d) - pow(sigma,2-d))/(2-d)) / (1-d);
        }
}

// function to be called from R
void C_polyiso_powerlaw(
        double *x, double *y,               // vertex coordinates (open)
        int *L,                             // number of vertices
        //intrfr_fn intrfr,                 // F(R)
        double *pars,                       // parameters for F(R)
        double *center_x, double *center_y, // center of isotropy
        int *subdivisions, double *epsabs, double *epsrel, // Rdqags options
        int *stop_on_error,                 // !=0 means to stop at first ier > 0
        double *value, double *abserr, int *neval) // results
{
        polyCub_iso(x, y, L,
                    intrfr_powerlaw,
                    pars, center_x, center_y,
                    subdivisions, epsabs, epsrel,
                    stop_on_error,
                    value, abserr, neval);
        return;
}
