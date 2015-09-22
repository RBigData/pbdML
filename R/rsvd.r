rsvd.checkargs <- function(x, k, q, retu, retvt)
{
  assert.type(retu, "logical")
  assert.type(retvt, "logical")
  
  assert.natnum(k)
  if (k > nrow(x))
    comm.stop("'k' must be no greater than nrow(x)")
  
  assert.natnum(q)
  
  ### TODO check for NA, NaN, Inf
  
  invisible(TRUE)
}



#' Random SVD
#' 
#' @param x
#' The input data matrix.
#' @param k
#' 
#' @param q
#' 
#' @param retu
#' Logical; should the left singular vectors ("U") be returned?
#' @param retvt
#' Logical; should the transposed right singular vectors ("VT") be returned?
#' 
#' @return
#' A list cotaining the singular values, and, if requested, the
#' left and/or right singular vectors.
#' 
#' @references
#' Halko, Martinsson, and Tropp. 2011. Finding structure with 
#' randomness: probabilistic algorithms for constructing approximate
#' matrix decompositions. SIAM Review 53 217-288.
#' 
#' @keywords SVD PCA
#' @name rsvd
#' @rdname rsvd
#' @export
rsvd <- function(x, k=1, q=3, retu=TRUE, retvt=TRUE)
{
  rsvd.checkargs(x=x, k=k, q=q, retu=retu, retvt=retvt)
  
  if (class(x) != "ddmatrix")
    x <- as.matrix(x)
  
  k <- as.integer(k)
  q <- as.integer(q)
  
  
  ### Stage A from the paper
  n <- ncol(x)
  
  if (class(x) == "matrix")
    Omega <- matrix(rnorm(n*2L*k), nrow=n, ncol=2L*k)
  else if (class(x) == "ddmatrix")
    Omega <- ddmatrix("rnorm", nrow=n, ncol=2L*k, bldim=x@bldim, ICTXT=x@ICTXT)
  
  Y <- x %*% Omega
  Q <- qr.Q(qr(Y))
  tx <- t(x)
  
  for (i in 1:q)
  {
    Y <- tx %*% Q
    Q <- qr.Q(qr(Y))
    Y <- x %*% Q
    Q <- qr.Q(qr(Y))
  }
  
  
  ### Stage B
  B <- t(Q) %*% x
  
  if (!retu)
    nu <- 0
  else
    nu <- min(nrow(B), ncol(B))
  
  if (!retvt)
    nv <- 0
  else
    nv <- min(nrow(B), ncol(B))
  
  svd.B <- La.svd(x=B, nu=nu, nv=nv)
  
  d <- svd.B$d
  d <- d[1L:k]
  
  
  # Produce u/vt as desired
  if (retu)
  {
    u <- svd.B$u
    u <- Q %*% u
    
    u <- u[, 1L:k]
  }
  
  if (retvt)
  {
    vt <- svd.B$vt[1L:k, ]
  }
  
  # wrangle return
  if (retu)
  {
    if (retvt)
      svd <- list(d=d, u=u, vt=vt)
    else
      svd <- list(d=d, u=u)
  }
  else
  {
    if (retvt)
      svd <- list(d=d, vt=vt)
    else
      svd <- list(d=d)
  }
  
  return( svd )
}

