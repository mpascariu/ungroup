// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
using namespace Rcpp;

using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::SparseMatrix;

// [[Rcpp::export]]
SEXP asSparseMat(const Eigen::Map<Eigen::MatrixXd> X) {
  SparseMatrix<double> Xsparse = X.sparseView();
  return(wrap(Xsparse));
}

// [[Rcpp::export]]
SEXP pclm_loop(const Eigen::MappedSparseMatrix<double> C,
               const Eigen::Map<Eigen::MatrixXd> P,
               const Eigen::Map<Eigen::MatrixXd> B,
               const Eigen::Map<Eigen::VectorXd> y,
               double maxiter, 
               double tol) {
  
  // initialize loop variables
  int ny = y.size();
  int kB = B.cols();
  double mua = std::log(y.sum()/ny);
  VectorXd mu = B * VectorXd::Constant(kB, mua).array().exp().matrix();
  VectorXd muA = C * mu;
  SparseMatrix<double> W;
  VectorXd z;
  MatrixXd Q;
  VectorXd Qz;
  MatrixXd QmQ;
  MatrixXd QmQP;
  VectorXd eta = mu.array().log().matrix();
  double d = 1000;
  double dd;
  
  // iterate until fit is good or no further fit improvements or max iterations
  for (int i = 0; i < maxiter; ++i) {
    W = C.cwiseProduct(muA.cwiseInverse() * mu.transpose());
    z = (y - muA) + (C * mu.cwiseProduct(eta));
    Q = (W * B).transpose();
    Qz = Q * z;
    QmQ = Q * (muA.asDiagonal() * Q.transpose());
    QmQP = QmQ + P;
    eta = B * QmQP.householderQr().solve(Qz);
    mu = eta.array().exp();
    muA = C * mu;
    dd = (y-muA).cwiseQuotient(y).cwiseAbs().mean() - d;
    d = d + dd;
    dd = std::abs(dd)/d;
    if((d < tol || dd < 0.001) && i >= 3) break;
  }
  
  // list output of complete function environment
  return List::create(
    Named("eta") = eta,
    Named("mu") = mu,
    Named("muA") = muA,
    Named("QmQ") = QmQ,
    Named("QmQP") = QmQP
  );
}
