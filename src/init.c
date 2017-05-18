/*******************************************************************************
// Registering native routines (entry points in compiled code)
// 
// This code is based on
// R-3.4.0> tools::package_native_routine_registration_skeleton("..")
//
// NOTE: R_forceSymbols(dll, TRUE);  // would require R >= 3.0.0
*******************************************************************************/

#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#include "polyCub.iso.h"

/* .C calls */
extern void C_polygauss(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"C_polygauss", (DL_FUNC) &C_polygauss, 13},
    {NULL, NULL, 0}
};

void R_init_polyCub(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("polyCub", "polyCub_iso", (DL_FUNC) &polyiso);
}
