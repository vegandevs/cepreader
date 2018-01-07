#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(cepclose)();
extern void F77_NAME(cepcond)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cepfree)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cephead)(void *, void *, void *, void *, void *);
extern void F77_NAME(cepnames)(void *);
extern void F77_NAME(cepopen)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"cepclose",  (DL_FUNC) &F77_NAME(cepclose),   0},
    {"cepcond",   (DL_FUNC) &F77_NAME(cepcond),   11},
    {"cepfree",   (DL_FUNC) &F77_NAME(cepfree),    9},
    {"cephead",   (DL_FUNC) &F77_NAME(cephead),    5},
    {"cepnames",  (DL_FUNC) &F77_NAME(cepnames),   1},
    {"cepopen",   (DL_FUNC) &F77_NAME(cepopen),   10},
    {NULL, NULL, 0}
};

void R_init_cepreader(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
