#' Random PCA
#' 
#' @param x
#' The input data matrix.
#' @param k
#' 
#' @param q
#' 
#' @param retx
#' 
#' @param center,scale
#' Logical; determines if the data should be centered/scaled first.
#' 
#' @return
#' An object of class \code{prcomp}.
#' 
#' @keywords SVD PCA
#' @name rpca
#' @rdname rpca
#' @export
rpca <- function(x, k=1, q=3, retx=TRUE, center=TRUE, scale=FALSE)
{
  if (class(x) != "ddmatrix")
    x <- as.matrix(x)
  
  assert.type(retx, "logical")
  assert.type(center, "logical")
  assert.type(scale, "logical")
  
  rsvd.checkargs(k=k, q=q, retu=FALSE, retvt=TRUE)
  
  assert.type(x, "numeric")
  
  if (center || scale)
    x <- scale(x, center=center, scale=scale)
  
  svd <- rsvd(x=x, k=k, q=q, retu=FALSE, retvt=TRUE)
  svd$d <- svd$d / sqrt(nrow(x) - 1L)
  
  pca <- list(sdev=svd$d, rotation=t(svd$vt))
  if (retx)
    pca$x <- x[1:k, ] %*% pca$rotation
  class(pca) <- "prcomp"
  
  return(pca)
}

