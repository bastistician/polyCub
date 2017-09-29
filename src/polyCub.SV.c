/*******************************************************************************
 * C-version of .polygauss.side()
 *
 * Copyright (C) 2014,2017 Sebastian Meyer
 *
 * This file is part of the R package "polyCub",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at https://www.R-project.org/Licenses/.
 ******************************************************************************/

#include "polyCub.SV.h"

static void C_polygauss_side(
    double *x1, double *y1, double *x2, double *y2,
    double *s_loc, double *w_loc, double *s_N, double *w_N,
    double *alpha,
    int *loc, int *N, // lengths (loc is M=N+1 or N)
    // *loc * *N nodes and weights will be computed
    double *nodes_x, double *nodes_y, double *weights)
{
    double half_pt_x     = (*x1 + *x2) / 2.0;
    double half_length_x = (*x2 - *x1) / 2.0;
    double half_pt_y     = (*y1 + *y2) / 2.0;
    double half_length_y = (*y2 - *y1) / 2.0;

    double x_gauss_side, y_gauss_side, scaling_fact_minus;
    int idx;
    for (int i = 0; i < *loc; i++) {
	// GAUSSIAN POINTS ON THE SIDE
	x_gauss_side = half_pt_x + half_length_x * s_loc[i];
	y_gauss_side = half_pt_y + half_length_y * s_loc[i];
	scaling_fact_minus = (x_gauss_side - *alpha) / 2.0;
	// COMPUTE NODES AND WEIGHTS
	for (int j = 0; j < *N; j++) {	
	    idx = j * *loc + i; // use same order as in R implementation
	    nodes_x[idx] = *alpha + scaling_fact_minus * (s_N[j] + 1.0);
	    nodes_y[idx] = y_gauss_side;
	    weights[idx] = half_length_y*scaling_fact_minus * w_loc[i] * w_N[j];
	}
    }
}


/***
 * Function to be called from R to loop over all polygon edges,
 * calling the above C_polygauss_side() for each
 ***/

void C_polygauss(
    double *x, double *y,     // vertex coordinates (open) of a polygon
    double *s_M, double *w_M, // nodes & weights of Gauss-Legendre quadrature 
    double *s_N, double *w_N, // of degree M=N+1 and N, respectively
    double *alpha,            // base-line
    int *L, int *M, int *N,   // L: number of edges/vertices
    // result: nodes and weights of length (<=) M*N per edge
    double *nodes_x, double *nodes_y, double *weights)
{
    int idxTo, idxBlock;
    double x1, y1, x2, y2;
    for (int i = 0; i < *L; i++) {

	x1 = x[i]; y1 = y[i];
	if (i == *L-1) idxTo = 0; else idxTo = i+1;
	x2 = x[idxTo]; y2 = y[idxTo];

	// if edge is on base-line or is orthogonal to it -> skip
	if ((x1 == *alpha && x2 == *alpha) || (y2 == y1))
	    continue;

	idxBlock = i * *M * *N; // start index of nodes of edge i
	if (x2 == x1)
	    // side is parallel to base-line -> use degree N in both dimensions
	    C_polygauss_side(&x1, &y1, &x2, &y2,
			     s_N, w_N, s_N, w_N, alpha,
			     N, N, 
			     nodes_x + idxBlock, nodes_y + idxBlock, weights + idxBlock);
	else
	    // use degrees M and N, respectively
	    C_polygauss_side(&x1, &y1, &x2, &y2,
			     s_M, w_M, s_N, w_N, alpha,
			     M, N, 
			     nodes_x + idxBlock, nodes_y + idxBlock, weights + idxBlock);

    }
}
