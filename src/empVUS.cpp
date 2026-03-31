#include <Rcpp.h>
using namespace Rcpp;

static inline double indvus(double a, double b, double c) {
  if(a < b && b < c) return 1.0;
  else return 0.0;
}

// [[Rcpp::export]]
double vusC_U(NumericVector tt1, NumericVector tt2, NumericVector tt3){
  int nn1 = tt1.size(), nn2 = tt2.size(), nn3 = tt3.size();
  double num = 0.0;
  for(int i = 0; i < nn1; i++){
    for(int j = 0; j < nn2; j++){
      for(int k = 0; k < nn3; k++){
        num += indvus(tt1[i], tt2[j], tt3[k]);
      }
    }
  }
  double out = num/(nn1*nn2*nn3);
  return out;
}

// [[Rcpp::export]]
NumericVector vusC_varEL(NumericVector tt1, NumericVector tt2,
                         NumericVector tt3){
  int nn1 = tt1.size(), nn2 = tt2.size(), nn3 = tt3.size();
  double num1 = 0.0;
  double num2 = 0.0;
  double num3 = 0.0;
  NumericVector U1(nn1);
  NumericVector U2(nn2);
  NumericVector U3(nn3);
  for(int i = 0; i < nn1; i++){
    for(int j = 0; j < nn2; j++){
      for(int k = 0; k < nn3; k++){
        U1[i] += indvus(tt1[i], tt2[j], tt3[k]);
      }
    }
    num1 += pow(U1[i], 2.0);
  }
  //
  for(int j = 0; j < nn2; j++){
    for(int i = 0; i < nn1; i++){
      for(int k = 0; k < nn3; k++){
        U2[j] += indvus(tt1[i], tt2[j], tt3[k]);
      }
    }
    num2 += pow(U2[j], 2.0);
  }
  //
  for(int k = 0; k < nn3; k++){
    for(int i = 0; i < nn1; i++){
      for(int j = 0; j < nn2; j++){
        U3[k] += indvus(tt1[i], tt2[j], tt3[k]);
      }
    }
    num3 += pow(U3[k], 2.0);
  }
  NumericVector out(3);
  double nn1_sq = pow(nn1, 2.0);
  double nn2_sq = pow(nn2, 2.0);
  double nn3_sq = pow(nn3, 2.0);
  out[0] = num1/(nn1*nn2_sq*nn3_sq);
  out[1] = num2/(nn2*nn1_sq*nn3_sq);
  out[2] = num3/(nn3*nn1_sq*nn2_sq);
  return out;
}

// [[Rcpp::export]]
List vusC_full_core(NumericVector tt1, NumericVector tt2, NumericVector tt3){
  int nn1 = tt1.size(), nn2 = tt2.size(), nn3 = tt3.size();
  NumericVector out1(nn1), out2(nn2), out3(nn3);
  NumericMatrix I_ij(nn2, nn1);
  NumericMatrix I_ik(nn3, nn1);
  for(int i = 0; i < nn1; i++){
    NumericMatrix M(nn3, nn2);
    for(int j = 0; j < nn2; j++){
      for(int k = 0; k < nn3; k++){
        M(k,j) = indvus(tt1[i], tt2[j], tt3[k]);
      }
    }
    NumericVector temp = colSums(M);
    I_ij(_, i) = temp;
    I_ik(_, i) = rowSums(M);
    out1[i] = sum(temp);
  }
  out2 = rowSums(I_ij);
  out3 = rowSums(I_ik);
  List out;
  out["ind1"] = out1;
  out["ind2"] = out2;
  out["ind3"] = out3;
  return out;
}

// [[Rcpp::export]]
NumericVector place_U(NumericVector tt1, NumericVector tt2, NumericVector tt3){
  int nn1 = tt1.size(), nn2 = tt2.size(), nn3 = tt3.size();
  NumericVector Ui(nn2);
  for(int j = 0; j < nn2; j++){
    for(int i = 0; i < nn1; i++){
      for(int k = 0; k < nn3; k++){
        Ui[j] += indvus(tt1[i], tt2[j], tt3[k]);
      }
    }
  }
  return Ui/(nn1*nn3);
}

// Version with ties
static inline double indvus_ties(double a, double b, double c) {
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
        num += indvus_ties(tt1[i], tt2[j], tt3[k]);
      }
    }
  }
  double out = num/(nn1*nn2*nn3);
  return out;
}

