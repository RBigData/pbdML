#' Machine Learning
#' 
#' TODO
#' 
#' \tabular{ll}{ 
#'    Package: \tab pbdML \cr 
#'    Type: \tab Package \cr 
#'    License: \tab BSD 2-clause \cr 
#'    LazyLoad: \tab yes \cr 
#' } 
#' 
#' This package requires an MPI library (OpenMPI, MPICH2, or LAM/MPI).
#' 
#' @importFrom pbdMPI allreduce comm.stop comm.print comm.cat comm.all
#' @importFrom stats runif
#' @import pbdDMAT
#' 
#' @useDynLib pbdML R_check_badvals R_check_groupvar R_one_norm R_shrink_op
#' 
#' @name pbdML-package
#' @docType package
#' @author Drew Schmidt \email{schmidt AT math.utk.edu}, George Ostrouchov, and Wei-Chen Chen.
#' @references Programming with Big Data in R Website: \url{http://r-pbd.org/}
#' @keywords Package
NULL
