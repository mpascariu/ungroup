// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
using namespace Rcpp;
using namespace RcppEigen;

using Eigen::Map;
using Eigen::MatrixXd; // dynamic double matrix
using Eigen::VectorXd; // dynamic double vector
using Eigen::MappedSparseMatrix; // dynamic double vector
using Eigen::SparseMatrix;
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


// -----------------------
// C++ Code by Jonas Scholey

// [[Rcpp::export]]
SEXP pclmloop(const Eigen::MappedSparseMatrix<double> C,
             const Eigen::Map<Eigen::MatrixXd> P,
             const Eigen::Map<Eigen::MatrixXd> B,
             const Eigen::Map<Eigen::VectorXd> y,
             double maxiter, 
             double tol) {
  
  // initialize loop variables
  int ny = y.size();
  int kB = B.cols();
  double mua = log(y.sum()/ny);
  VectorXd mu = B*VectorXd::Constant(kB, mua).array().exp().matrix();
  VectorXd muA = C*mu;
  SpMat W;
  VectorXd z;
  MatrixXd Q;
  VectorXd Qz;
  MatrixXd QmQ;
  MatrixXd QmQP;
  VectorXd eta;
  double d = 1000;
  double dd;
  
  // iterate until fit is good or no further fit improvements or max iterations
  for (int i = 0; i < maxiter; ++i) {
    W = C.cwiseProduct(muA.cwiseInverse() * mu.transpose());
    z = (y.array() - muA.array()) + (C * (mu.array()*(mu.array().log())).matrix()).array();
    Q = (W * B).transpose();
    Qz = Q * z;
    QmQ = Q * (Q.transpose().array().colwise() * muA.array()).matrix();
    QmQP = QmQ + P;
    eta = B * QmQP.colPivHouseholderQr().solve(Qz);
    mu = eta.array().exp();
    muA = C * mu;
    dd = (((y.array()-muA.array())/y.array()).abs().mean())-d;
    d = d + dd;
    dd = std::abs(dd)/d;
    if((d < tol || dd < 0.001) && i >= 3) break;
  }
  
  // List output of complete function environment
  return List::create(
    // fitting results
    Named("mu") = mu,
    Named("muA") = muA,
    // intermediate results
    Named("W") = W,
    Named("z") = z,
    Named("Q") = Q,
    Named("Qz") = Qz,
    Named("QmQ") = QmQ,
    Named("QmQP") = QmQP,
    Named("eta") = eta,
    // fit info
    Named("d") = d,
    Named("dd") = dd
  );
}
