# Reference: "Robust Principal Component Analysis?" https://arxiv.org/pdf/0912.3599.pdf

# sum(abs(X))
one_norm = function(X)
{
  if (is.ddmatrix(X))
  {
    ret = .Call(R_one_norm, X@Data)
    allreduce(ret)
  }
  else
    .Call(R_one_norm, X)
}



## \mathcal{S} from the paper - sign(X) * pmax(abs(X) - tau, 0)
shrink_op = function(X, tau)
{
  # NOTE: this modifies the memory in place, which is potentially very dangerous
  if (is.ddmatrix(X))
    .Call(R_shrink_op, X@Data, tau)
  else
    .Call(R_shrink_op, X, tau)
}



## \mathcal{D} from the paper
sv_thresh = function(X, tau)
{
  decomp = La.svd(X)
  
  sigma = decomp$d
  shrink_op(sigma, tau)
  U = decomp$u
  Vt = decomp$vt
  
  U %*% (sigma * Vt)
}



#' robsvd
#' 
#' Implementation of the robust pca algorithm.
#' 
#' @description
#' The optimization problem is solved by an alternating directions technique.
#' 
#' @param M
#' The input data, stored as a numeric matrix or ddmatrix.
#' @param delta
#' Numeric termination criteria. A smaller (closer to 0) value will require more
#' iterations. See the summary following the Algorithm 1 listing in the
#' referenced paper for details.
#' @param maxiter
#' Maximum number of iterations. Should at least be a few hundred.
#' 
#' @references
#' Candès, E.J., Li, X., Ma, Y. and Wright, J., 2011. Robust principal component
#' analysis?. Journal of the ACM (JACM), 58(3), p.11.
#' 
#' @examples
#' \dontrun{
#' m = 10
#' n = 3
#' M = matrix(rnorm(m*n), m)
#' robsvd(M)
#' }
#' 
#' @author
#' Drew Schmidt
#' 
#' @export
robsvd = function(M, delta=1e-7, maxiter=1000)
{
  ### I love dynamic typing
  assert.type(delta, "numeric")
  assert.posint(maxiter)
  
  if (class(x) != "ddmatrix")
  {
    x <- as.matrix(x)
    
    if (!is.double(M))
      storage.mode(M) <- "double"
  }
  
  
  ### the actual work
  n1 = nrow(M)
  n2 = ncol(M)
  
  lambda = 1/sqrt(max(n1, n2))
  
  mu = 0.25 * n1*n2 / one_norm(M)
  
  S = matrix(0, n1, n2)
  Y = matrix(0, n1, n2)
  
  conv = FALSE
  iter = 0L
  
  ub = delta * norm(M, "F")
  
  while (!conv && iter < maxiter)
  {
    if (iter == 0)
      L = sv_thresh(M, 1/mu)
    else
      L = sv_thresh(M - S + Y, 1/mu)
    
    tmp = M - L
    S = tmp + Y
    shrink_op(S, lambda/mu)
    
    tmp = tmp - S
    Y = Y + tmp
    
    term = norm(tmp, "F")
    conv = (term <= ub)
    iter = iter + 1L
  }
  
  info = list(iterations=iter, converged=iter<maxiter, term=term)
  list(L=L, S=S, info=info)
}
