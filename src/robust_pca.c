// robust pca utilities

#include <R.h>
#include <Rinternals.h>
#include <math.h>

#define SIGN(x) ((x)>0?1:-1)

#define MAX(a,b) ((a)>(b)?(a):(b))


SEXP R_one_norm(SEXP x_)
{
  SEXP ret;
  double norm = 0.0;
  int i, j;
  const int m = nrows(x_);
  const int n = ncols(x_);
  const double *const restrict x = REAL(x_);
  
  // sum(abs(x))
  for (j=0; j<n; j++)
  {
    for (i=0; i<m; i++)
      norm += fabs(x[i + m*j]);
  }
  
  PROTECT(ret = allocVector(REALSXP, 1));
  REAL(ret)[0] = norm;
  UNPROTECT(1);
  return ret;
}



// NOTE: this modifies the memory in place, which is potentially very dangerous
SEXP R_shrink_op(SEXP x_, SEXP tau_)
{
  int i, j;
  const int m = nrows(x_);
  const int n = ncols(x_);
  const double tau = REAL(tau_)[0];
  
  PROTECT(x_);
  double *const restrict x = REAL(x_);
  
  // sign(X) * pmax(abs(X) - tau, 0)
  for (j=0; j<n; j++)
  {
    for (i=0; i<m; i++)
      x[i + m*j] = SIGN(x[i + m*j]) * MAX(fabs(x[i + m*j]) - tau, 0);
  }
  
  
  UNPROTECT(1);
  return R_NilValue;
}
