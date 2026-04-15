#include <Rcpp.h>
using namespace Rcpp;

// Version with ties
static inline double ind_vus_ties(double a, double b, double c) {
  if(a > b || c < b) return 0.0;
  else{
    double d = 0.0;
    if(a == b) d += 1.0;
    if(c == b) d += 1.0;
    return (8 - 3*d)/(8 + 2*d);
  }
}

// [[Rcpp::export]]
double vusC_ties(NumericVector tt1, NumericVector tt2, NumericVector tt3){
  int nn1 = tt1.size(), nn2 = tt2.size(), nn3 = tt3.size();
  double num = 0.0;
  for(int i = 0; i < nn1; i++){
    for(int j = 0; j < nn2; j++){
      for(int k = 0; k < nn3; k++){
        num += ind_vus_ties(tt1[i], tt2[j], tt3[k]);
      }
    }
  }
  double out = num/nn1/nn2/nn3;
  return out;
}

