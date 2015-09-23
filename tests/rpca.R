suppressPackageStartupMessages(library(pbdML))


### non-mpi test only
x <- matrix(1:30, 10)

pca_est <- rpca(x)

# truncate full
pca_full <- prcomp(x)
pca_full$sdev <- pca_full$sdev[1]
pca_full$rotation <- pca_full$rotation[, 1, drop=FALSE]
pca_full$center <- pca_full$center[1]
pca_full$x <- pca_full$x[, 1, drop=FALSE]

# account for sign
if (sign(pca_full$rotation[1,1]) != sign(pca_est$rotation[1,1]))
{
  pca_full$rotation <- -pca_full$rotation
  pca_full$x <- -pca_full$x
}



stopifnot(all.equal(pca_est, pca_full))
