# pbdML

* **Version:** 0.1-2
* **URL**: https://github.com/RBigData/pbdML
* **License:** [![License](http://img.shields.io/badge/license-BSD%202--Clause-orange.svg?style=flat)](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** See section below.


**pbdML** is an R package containing a collection of machine learning utilities.  These functions can be used in serial with native R objects (matrices and vectors) or in parallel with distributed matrices from the **pbdDMAT** package.  Here, we focus on ease of coding and understanding rather than performance (i.e., all code is written in R), and as such this package should primarily be thought of as a demonstration of the capabilities of the **pbdDMAT** package.



## Usage

Functions have the same dispatch whether working with a regular
R matrix or with a `ddmatrix` from the pbdDMAT package.

So for example, to compute a randomized PCA, we can run

```r
rpca(x)
```

where `x` is either a matrix or a `ddmatrix`.



## Installation
**pbdML** requires:

* R version 3.0.0 or higher
* A system installation of MPI
* The **pbdMPI** and **pbdDMAT** packages, as well as their dependencies.

<!-- #### Stable Version
```r
install.packages("pbdCS")
``` -->

#### Development Version
```r
remotes::install_github("RBigData/pbdML")
```




## Authors

pbdML is authored and maintained by members of the pbdR core team:
* Drew Schmidt
* George Ostrouchov
* Wei-Chen Chen
