suppressPackageStartupMessages(library(pbdML))
comm.set.seed(12345)

### non-mpi test only
m <- 100
n <- 10

x <- matrix(rnorm(m*n), m, n)
g <- sample(0:1, size=m, replace=TRUE, prob=c(.25, .75))

truth <- 0.224449140419224
test <- fld(x, g)$c

stopifnot(all.equal(truth, test))


#dx = as.ddmatrix(x)
#dg = as.ddmatrix(g)

#test <- fld(dx, dg)$c
#stopifnot(all.equal(truth, test))
