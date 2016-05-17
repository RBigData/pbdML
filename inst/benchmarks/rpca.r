library(rbenchmark)
suppressPackageStartupMessages(library(pbdML))

### non-mpi test only
m <- 1000
n <- 100
x <- rnorm(m*n)
dim(x) <- c(m, n)


benchmark(rpca(x, k=1))

