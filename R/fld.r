#' Fisher's Linear Discriminant
#' 
#' Compute the 2-class Fisher's linear discriminant either in 
#' serial or parallel.
#' 
#' @details
#' Note that a ddmatrix x and ordinary matrix/vector pairing is possible, 
#' but the reverse (matrix/vector x and ddmatrix g) is not.
#' 
#' @param x
#' The data in the form of a matrix or ddmatrix.
#' @param g
#' The group variable in the form of a matrix/vector or a ddmatrix.
#' The values should be 0 and 1 exclusively.
#' @param checkg
#' Logical; should the values of g be confirmed to exclusively be 0's
#' and 1's.
#' 
#' @return
#' A list of class 'fld' containing the prior probabilities, group means, 
#' w vector, and c scalar.  In the distributed case, the priors and c scalar
#' are both global, while the other values are distributed.
#' 
#' @examples
#' \dontrun{
#' x <- matrix(rnorm(30), 10)
#' g <- sample(0:1, size=10, replace=TRUE)
#' 
#' fld(x, g)
#' }
#' 
#' @name fld
#' @rdname fld
#' @export
fld <- function(x, g, checkg=TRUE)
{
  if (checkg)
  {
    if (any(g != 0 && g != 1))
      comm.stop("argument 'g' must be ")
  }
  
  if (!is.ddmatrix(x))
  {
    if (is.ddmatrix(g))
      comm.stop("a matrix/vector 'x' can not be used with a ddmatrix 'g'; see '?fld' for details")
    
    x <- as.matrix(x)
  }
  if (!is.ddmatrix(g))
    g <- as.matrix(g)
  
  
  n <- nrow(x)
  
  if (n != nrow(g))
    comm.stop("argument 'g' must be the same length as 'x'")
  
  ### Get group indices/priors
  #FIXME use table()
  if (is.ddmatrix(g))
  {
    ind0 <- which(g@Data == 0)
  }
  else
    ind0 <- which(g==0)
  
  ind1 <- setdiff(1:n, ind0)
  
  prior0 <- length(ind0)/n
  prior1 <- length(ind1)/n
  
  ### Get group covariances and means
  x0 <- x[ind0, , drop=FALSE]
  x1 <- x[ind1, , drop=FALSE]
  
  cov0 <- cov(x0)
  cov1 <- cov(x1)
  
  mu0 <- colMeans(x0)
  mu1 <- colMeans(x1)
  
  ### fld
  mu_sum <- mu0 + mu1
  
  w <- solve(cov0 + cov1, mu_sum)
  c <- as.vector(0.5 * crossprod(w, mu_sum))
  
  ### wrangle return
  means <- list(mu0=mu0, mu1=mu1)
  prior <- c("0"=prior0, "1"=prior1)
  
  ret <- list(prior=prior, means=means, w=w, c=c)
  class(ret) <- "fld"
  
  return(ret)
}



#' @name print-fld
#' @method print fld
#' @export
print.fld <- function(x, ...)
{
  comm.cat("Prior probabilities of groups:\n", quiet=TRUE)
  comm.print(x$prior, quiet=TRUE)
  
  comm.cat("\nc =", x$c, "\n", quiet=TRUE)
}

