#include <RcppEigen.h>
using namespace Rcpp;

using Eigen::Map;
using Eigen::MatrixXd; // double matrix

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
SEXP tMatMult(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B){
    MatrixXd C = A * B;
    MatrixXd Ct = C.transpose();
    return wrap(Ct);
}
