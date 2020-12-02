#include <RcppEigen.h>

// [[Rcpp::depends(BH, bigmemory)]]
#include <bigmemory/MatrixAccessor.hpp>

#include <numeric>
// [[Rcpp::depends(RcppEigen)]]

using Eigen::Map;                       // 'maps' rather than copies
using Eigen::MatrixXd;                  // variable size matrix, double precision
using Eigen::VectorXd;                  // variable size vector, double
using namespace Rcpp;

// [[Rcpp::export]]
VectorXd solveCppLU(Map<MatrixXd> A, Map<VectorXd> x) {
  return A.lu().solve(x);
}

// [[Rcpp::export]]
MatrixXd solveMatCppLU(Map<MatrixXd> A, Map<MatrixXd> X) {
  return A.lu().solve(X);
}

// [[Rcpp::export]]
VectorXd solveCppLLT(Map<MatrixXd> A, Map<VectorXd> x) {
  return A.llt().solve(x);
}

// [[Rcpp::export]]
VectorXd solveCppLDLT(Map<MatrixXd> A, Map<VectorXd> x) {
  return A.ldlt().solve(x);
}

// [[Rcpp::export]]
MatrixXd solveMatCppLDLT(Map<MatrixXd> A, Map<MatrixXd> X) {
  return A.ldlt().solve(X);
}

// [[Rcpp::export]]
MatrixXd solveMatCppLLT(Map<MatrixXd> A, Map<MatrixXd> X) {
  return A.llt().solve(X);
}

// [[Rcpp::export]]
MatrixXd invCpp(Map<MatrixXd> A) {
  return A.inverse();
}


