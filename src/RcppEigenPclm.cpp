// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <climits>
using namespace Rcpp;

using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::SparseMatrix;

// Every reciprocal is taken against at least this value. An expected bin total
// below it would otherwise divide by zero and poison the working weights.
const double MU_FLOOR = 1e-12;

// Bounds on the linear predictor before it is exponentiated. exp() overflows to
// Inf a little above 700, and one Inf reaches every later iteration at once.
const double ETA_MAX = 700.0;


// [[Rcpp::export]]
SEXP asSparseMat(SEXP X) {
  // RcppEigen's dense map reads a plain vector as an n x 1 matrix without
  // complaint, so the shape has to be checked before anything is built from it.
  if (!Rf_isMatrix(X)) {
    Rcpp::stop("asSparseMat: 'X' must be a matrix");
  }
  NumericMatrix Xm(X);
  Eigen::Map<Eigen::MatrixXd> M(Xm.begin(), Xm.nrow(), Xm.ncol());
  SparseMatrix<double> Xsparse = M.sparseView();
  return wrap(Xsparse);
}


// [[Rcpp::export]]
SEXP pclm_loop(const Eigen::Map<Eigen::SparseMatrix<double>> C,
               const Eigen::Map<Eigen::MatrixXd> P,
               const Eigen::Map<Eigen::MatrixXd> B,
               const Eigen::Map<Eigen::VectorXd> y,
               double maxiter, 
               double tol) {
  
  // -- scalars -----------------------------------------------------------
  if (!R_FINITE(maxiter) || maxiter < 1.0 || maxiter > INT_MAX) {
    Rcpp::stop("pclm_loop: 'maxiter' must be a finite value in [1, INT_MAX]");
  }
  if (!R_FINITE(tol) || tol <= 0.0) {
    Rcpp::stop("pclm_loop: 'tol' must be a positive, finite value");
  }
  const int maxit = static_cast<int>(maxiter);
  
  // -- shapes ------------------------------------------------------------
  // R compiles packages with -DNDEBUG, which switches Eigen's own assertions
  // off, so a shape mismatch would otherwise read past R's memory.
  const int ny = static_cast<int>(y.size());
  const int n  = static_cast<int>(B.rows());
  const int kB = static_cast<int>(B.cols());
  if (C.rows() != ny) {
    Rcpp::stop("pclm_loop: nrow(C) must equal length(y)");
  }
  if (C.cols() != n) {
    Rcpp::stop("pclm_loop: ncol(C) must equal nrow(B)");
  }
  if (P.rows() != kB || P.cols() != kB) {
    Rcpp::stop("pclm_loop: 'P' must be square with ncol(B) rows and columns");
  }
  
  // -- counts ------------------------------------------------------------
  for (int i = 0; i < ny; ++i) {
    if (!R_FINITE(y[i])) {
      Rcpp::stop("pclm_loop: 'y' must hold only finite counts");
    }
    if (y[i] < 0.0) {
      Rcpp::stop("pclm_loop: 'y' must not hold negative counts");
    }
  }
  const double ysum = y.sum();
  if (ysum <= 0.0) {
    Rcpp::stop("pclm_loop: 'y' must hold at least one positive count");
  }
  
  // -- initialisation ----------------------------------------------------
  // A constant starting fit at the mean count. Written as one scalar times the
  // row sums of B rather than a constant vector pushed through a product, which
  // is the same number for a lot less work.
  const double mu0 = ysum / ny;
  VectorXd mu = B * VectorXd::Ones(kB);
  mu *= mu0;
  mu = mu.array().max(MU_FLOOR).matrix();
  VectorXd eta    = mu.array().log().matrix();
  VectorXd muA(ny), muA_inv(ny), z(ny), Qz(kB);
  MatrixXd Q(kB, ny), QmQ(kB, kB), QmQP(kB, kB);
  double d = 1000.0;
  
  // -- iterate -----------------------------------------------------------
  for (int i = 0; i < maxit; ++i) {
    if (i % 32 == 0) {
      Rcpp::checkUserInterrupt();
    }
    
    muA = C * mu;
    muA_inv = muA.cwiseMax(MU_FLOOR).cwiseInverse();
    
    // W would be C . (muA^-1 mu^T), a dense n x m product rebuilt every pass
    // only to serve the nonzeros of C. Only W * B is ever needed, and
    //     (W B)_ik = muA^-1_i sum_j C_ij mu_j B_jk
    // so one sparse times dense product and a row scaling replaces it.
    MatrixXd WB = C * (mu.asDiagonal() * B);
    WB.array().colwise() *= muA_inv.array();
    Q = WB.transpose();
    
    z   = (y - muA) + (C * mu.cwiseProduct(eta));
    Qz  = Q * z;
    QmQ = Q * (muA.asDiagonal() * Q.transpose());
    QmQP = QmQ + P;
    
    // QmQP is symmetric positive definite here: QmQ is a Gram matrix and P is
    // the difference penalty. LDLT costs about half of a general QR solve.
    Eigen::LDLT<Eigen::MatrixXd> ldlt(QmQP);
    if (ldlt.info() != Eigen::Success) {
      Rcpp::stop("pclm_loop: the penalized system could not be factorized");
    }
    VectorXd beta = ldlt.solve(Qz);
    if (ldlt.info() != Eigen::Success) {
      Rcpp::stop("pclm_loop: the penalized system could not be solved");
    }
    
    eta = B * beta;
    eta = eta.array().min(ETA_MAX).max(-ETA_MAX).matrix();
    mu  = eta.array().exp().matrix();
    muA = C * mu;
    
    // Mean absolute relative error of the fitted bin totals. The denominator is
    // floored at 1 because an empty bin is routine in binned counts, and one of
    // them used to turn the whole metric into NaN and silently disable early
    // stopping for the rest of the fit.
    const double d_new = (y - muA).cwiseQuotient(y.cwiseMax(1.0)).cwiseAbs().mean();
    const double rel   = (d_new > 0.0) ? std::abs(d_new - d) / d_new : 0.0;
    d = d_new;
    if ((d < tol || rel < 0.001) && i >= 3) break;
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
