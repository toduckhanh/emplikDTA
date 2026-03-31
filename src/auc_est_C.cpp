#include <Rcpp.h>
using namespace Rcpp;

static inline double ind_auc(double a, double b) {
  if(a > b) return 0.0;
  else return 1.0;
}

// [[Rcpp::export]]
double auc_U_C(NumericVector x, NumericVector y) {
  int n1 = x.size(), n2 = y.size();
  double res = 0.0;
  for(int i = 0; i < n1; i++){
    for(int j = 0; j < n2; j++){
      res += ind_auc(x[i], y[j]);
    }
  }
  return res/n1/n2;
}

// Version with ties
static inline double ind_auc_ties(double a, double b) {
  if(a > b) return 0.0;
  else{
    double d = 0.0;
    if(a == b) d += 1.0;
    return (8 - 3*d)/(8 + 2*d);
  }
}

// [[Rcpp::export]]
double auc_ties_C(NumericVector x, NumericVector y) {
  int n1 = x.size(), n2 = y.size();
  double res = 0.0;
  for(int i = 0; i < n1; i++){
    for(int j = 0; j < n2; j++){
      res += ind_auc_ties(x[i], y[j]);
    }
  }
  return res/n1/n2;
}
