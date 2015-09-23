#' Decompose/Recompose
#' 
#' Exclude the variance contributed by the principal components
#' enumerated in the vector \code{exclude} from the data matrix.
#' 
#' @param x
#' A matrix or ddmatrix of numeric values.
#' @param exclude
#' A vector of positive integer values which are the principal components
#' whose variance contribution should be removed.
#' @param center
#' Logical; determines if the matrix should be centered first.
#' @param scale
#' Logical; determines if the matrix should be scaled first.
#' 
#' @return
#' A matrix or ddmatrix, matching the same type as the input \code{x}.
#' 
#' @references
#' Inspired by a question from Ahmed Moustafa: \url{https://twitter.com/AhmedMoustafa/status/646310686725812224}
#' 
#' @author
#' Drew Schmidt and Ahmed Moustafa
#' 
#' @examples
#' \dontrun{
#' ## TODO add mpirun message boilerplate
#' library(pbdDMAT)
#' init.grid()
#' 
#' x <- ddmatrix("rnorm", nrow=100, ncol=10, bldim=c(2,2))
#' # Remove the contribution of PC 1's variance
#' decomp_recomp(x, 1)
#' 
#' finalize()
#' }
#' 
#' @export
decomp_recomp <- function(x, exclude, center=TRUE, scale=FALSE)
{
  if (class(x) != "matrix" && class(x) != "ddmatrix")
    comm.stop("Argument 'x' must be of class 'matrix' or 'ddmatrix'")
  
  if (any(exclude < 1))
    comm.stop("positive")
 
  assert.type(center, "logical")
  assert.type(scale, "logical")
  

  if (center || scale)
    x <- scale(x, center=center, scale=scale)
  
  svd <- La.svd(x)
  
  u <- svd$u
  d <- svd$d
  vt <- svd$vt
  
  ud <- sweep(u[, -exclude], MARGIN=2, FUN="*", STATS=d[-exclude])
  ud %*% vt[-exclude, ]
}

