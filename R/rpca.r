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
  
  # Check needs to be here so we don't do anything demanding before possibly needing to error out
  rsvd.checkargs(x=x, k=k, q=q, retu=FALSE, retvt=TRUE)
  
  assert.type(x, "numeric")
  
  if (center || scale)
    x <- scale(x, center=center, scale=scale)
  
  svd <- rsvd(x=x, k=k, q=q, retu=FALSE, retvt=TRUE)
  svd$d <- svd$d / sqrt(nrow(x) - 1L)
  
  if (center)
    center <- attr(x, "scaled:center")[1:k]
  if (scale)
    scale <- attr(x, "scaled:scale")[1:k]
  
  pca <- list(sdev=svd$d, rotation=t(svd$vt), center=center, scale=scale)
  
  if (is.matrix(x))
    colnames(pca$rotation) <- paste0("PC", 1:ncol(pca$rotation))
#  else #FIXME
  
  if (retx)
    pca$x <- x %*% pca$rotation
  class(pca) <- "prcomp"
  
  return(pca)
}

