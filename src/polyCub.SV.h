/*******************************************************************************
 * Header file of polyCub.SV.c
 *
 * Copyright (C) 2017 Sebastian Meyer
 *
 * This file is part of the R package "polyCub",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at https://www.R-project.org/Licenses/.
 ******************************************************************************/

void C_polygauss(
    double *x, double *y,     // vertex coordinates (open) of a polygon
    double *s_M, double *w_M, // nodes & weights of Gauss-Legendre quadrature 
    double *s_N, double *w_N, // of degree M=N+1 and N, respectively
    double *alpha,            // base-line
    int *L, int *M, int *N,   // L: number of edges/vertices
    // result: nodes and weights of length (<=) M*N per edge
    double *nodes_x, double *nodes_y, double *weights);
