# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Fri Jan 08 09:48:33 2021
# --------------------------------------------------- #

#' @details 
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "ungroup")}
#' \insertNoCite{*}{ungroup}
#' @references \insertAllCited{}
#' @importFrom Rcpp sourceCpp
#' @importFrom stats optimise qnorm quantile fitted aggregate nlminb AIC BIC
#' @importFrom utils tail
#' @importFrom graphics axis barplot legend lines abline par plot.default
#' @importFrom rgl axes3d box3d open3d surface3d title3d
#' @importFrom pbapply startpb setpb closepb
#' @import Rdpack
#' @importClassesFrom Matrix dgCMatrix
#' @name ungroup
#' @useDynLib ungroup
#' @aliases NULL
#' @docType package
"_PACKAGE"
