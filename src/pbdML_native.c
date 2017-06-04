/* Automatically generated. Do not edit by hand. */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP R_check_badvals(SEXP x_);
extern SEXP R_check_groupvar(SEXP g_);
extern SEXP R_one_norm(SEXP x_);
extern SEXP R_shrink_op(SEXP x_, SEXP tau_);

static const R_CallMethodDef CallEntries[] = {
  {"R_check_badvals", (DL_FUNC) &R_check_badvals, 1},
  {"R_check_groupvar", (DL_FUNC) &R_check_groupvar, 1},
  {"R_one_norm", (DL_FUNC) &R_one_norm, 1},
  {"R_shrink_op", (DL_FUNC) &R_shrink_op, 2},
  {NULL, NULL, 0}
};

void R_init_pbdML(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
