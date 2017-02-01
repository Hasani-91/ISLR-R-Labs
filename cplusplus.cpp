#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double rwUpdateC(double rw_prev, double volLag, double volLB, double volUB, double volTarget, double tradeSize) {
  double regearTemp; double percentDegear; double percentDegear_prev; double vol_port_prev;
  double dZero = 0.0;
  percentDegear_prev = 1.0 - rw_prev;
  vol_port_prev = volLag * rw_prev; 
  if (vol_port_prev > volUB) {
    percentDegear = (1.0 - volTarget / volLag);
  } else if (vol_port_prev < volLB) {
    if (std::abs(1 - volTarget / volLag - percentDegear_prev) > tradeSize) {
      regearTemp = -1.0 * tradeSize;
    } else {
      regearTemp = (1.0 - volTarget / volLag - percentDegear_prev);
    }
    percentDegear = std::max(percentDegear_prev + regearTemp, dZero);
  } else {
    percentDegear = percentDegear_prev;
  }
  return 1 - percentDegear;
}

// [[Rcpp::export]]
NumericVector rwIterateC(NumericVector volLag, double volLB, double volUB, double volTarget, double tradeSize) {
  int nv = volLag.size(); 
  NumericVector rw(nv);
  rw[0] = 1;
  for (int i = 1; i < nv; i++) {
    rw[i] = rwUpdateC(rw[i-1],volLag[i],volLB,volUB,volTarget,tradeSize);
  }
  return rw;
}