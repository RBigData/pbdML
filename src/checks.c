#include <R.h>
#include <Rinternals.h>


// Check for NA, Inf, and NaN
SEXP R_check_badvals(SEXP x_)
{
  SEXP ret;
  int check =  0;
  double *x = REAL(x_);
  
  for (int i=0; i<LENGTH(x_); i++)
  {
    if (!R_FINITE(x[i]))
      goto makeret;
  }
  
  check = 1;
  
  makeret:
    PROTECT(ret = allocVector(LGLSXP, 1));
    INTEGER(ret)[0] = check;
  
  return ret;
}



SEXP R_check_groupvar(SEXP g_)
{
  SEXP ret;
  const int n = LENGTH(g_);
  int *g = INTEGER(g_);
  int i;
  int test = 1;
  
  for (i=0; i<n; i++)
  {
    if (g[i] != 0 && g[i] != 1)
    {
      test = 0;
      break;
    }
  }
  
  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = test;
  UNPROTECT(1);
  return ret;
}
