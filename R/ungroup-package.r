# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Mon Dec 28 21:06:37 2020
# --------------------------------------------------- #

#' @details 
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "ungroup")}
#' \insertNoCite{*}{ungroup}
#' @references \insertAllCited{}
#' @importFrom Rcpp sourceCpp
#' @importFrom stats optimise qnorm quantile fitted aggregate nlminb AIC BIC
#' @importFrom utils tail
#' @importFrom MortalitySmooth MortSmooth_bbase
#' @importFrom graphics axis barplot legend lines plot abline par
#' @importFrom rgl axes3d box3d open3d surface3d title3d
#' @importFrom pbapply startpb setpb closepb
#' @import Rdpack
#' @importClassesFrom Matrix dgCMatrix
#' @name ungroup
#' @useDynLib ungroup
#' @aliases NULL
#' @docType package
"_PACKAGE"
