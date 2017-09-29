/*******************************************************************************
 * Registering native routines (entry points in compiled code)
 *
 * Copyright (C) 2017 Sebastian Meyer
 *
 * This file is part of the R package "polyCub",
 * free software under the terms of the GNU General Public License, version 2,
 * a copy of which is available at https://www.R-project.org/Licenses/.
 ******************************************************************************/

#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#include "polyCub.SV.h"
#include "polyCub.iso.h"

static const R_CMethodDef CEntries[] = {
    {"C_polygauss", (DL_FUNC) &C_polygauss, 13},
    {NULL, NULL, 0}
};

void R_init_polyCub(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    //R_forceSymbols(dll, TRUE);  // would require R >= 3.0.0

    R_RegisterCCallable("polyCub", "polyiso", (DL_FUNC) &polyiso);
}
