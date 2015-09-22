suppressPackageStartupMessages(library(pbdML))


### non-mpi test only
m <- 100
n <- 10

exclude <- 1

x <- matrix(rnorm(m*n), m, n)
x_dr <- decomp_recomp(x, exclude, center=FALSE, scale=FALSE)

pca.x <- prcomp(x)
pca.x_dr <- prcomp(x_dr)

u <- pca.x$u
d <- pca.x$d
vt <- pca.x$vt

u_dr <- pca.x_dr$u
d_dr <- pca.x_dr$d
vt_dr <- pca.x_dr$vt


stopifnot(all.equal(u_dr, u[, -exclude]))
stopifnot(all.equal(d_dr, d[-exclude]))
stopifnot(all.equal(vt_dr, vt[-exclude, ]))
