// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

using namespace Rcpp;
using namespace RcppEigen;
using  Eigen::MappedSparseMatrix;
using  Eigen::SparseMatrix;
typedef Eigen::SparseMatrix<double> SpMat;   
typedef Eigen::Map<Eigen::MatrixXd> MapMatd; 
typedef Eigen::MappedSparseMatrix<double> MSpMat;

// [[Rcpp::export]]
SEXP asSparseMat(SEXP inX){
  MapMatd X(Rcpp::as<MapMatd>(inX));
  SpMat Xsparse = X.sparseView();              
  S4 Xout(wrap(Xsparse));                      
  NumericMatrix Xin(inX);                      
  Xout.slot("Dimnames") = clone(List(Xin.attr("dimnames")));
  return(Xout);
}

// [[Rcpp::export]]
SEXP SparseProd(SEXP inX, SEXP inY){
  const SpMat X(as<MSpMat>(inX));
  const SpMat Y(as<MSpMat>(inY));
  return wrap(X * Y);
}
