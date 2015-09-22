#include <stdbool.h>
#include <R.h>
#include <Rinternals.h>


// Check for NA, Inf, and NaN
SEXP R_badVals(SEXP x_)
{
  SEXP ret;
  bool check =  false;
  double *x = REAL(x_);
  
  for (int i=0; i<LENGTH(x_); i++)
  {
    if (!R_FINITE(x[i]))
      goto makeret;
  }
  
  check = true;
  
  makeret:
    PROTECT(ret = allocVector(LGLSXP, 1));
    INTEGER(ret)[0] = check;
  
  return ret;
}

