#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rversion.h>


void R_init_sarsop(DllInfo *dll) {
  R_registerRoutines(dll, NULL, NULL, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
  R_useDynamicSymbols(dll, FALSE);
#endif
}
