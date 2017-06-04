# Reference: "Robust Principal Component Analysis?" https://arxiv.org/pdf/0912.3599.pdf


## \mathcal{S} from the paper
shrink_op = function(X, tau)
{
  sign(X) * pmax(abs(X) - tau, 0)
}



## \mathcal{D} from the paper
sv_thresh = function(X, tau)
{
  decomp = La.svd(X)
  
  sigma = shrink_op(decomp$d, tau)
  U = decomp$u
  Vt = decomp$vt
  
  U %*% (sigma * Vt)
}



#' robsvd
#' 
#' Implementation of the robust pca algorithm.
#' 
#' @description
#' TODO
#' 
#' @param M
#' TODO
#' @param delta
#' TODO
#' @param maxiter
#' TODO
#' 
#' @refreences
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
  n1 = nrow(M)
  n2 = ncol(M)
  
  lambda = 1/sqrt(max(n1, n2))
  
  mu = 0.25 * n1*n2 / norm(M, "1")
  
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
    S = shrink_op(tmp + Y, lambda/mu)
    
    tmp = tmp - S
    Y = Y + tmp
    
    term = norm(tmp, "F")
    conv = (term <= ub)
    iter = iter + 1L
  }
  
  info = list(iterations=iter, converged=iter<maxiter, term=term)
  list(L=L, S=S, info=info)
}
