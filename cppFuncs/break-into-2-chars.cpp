#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::CharacterVector break_into_two_charsCPP(const std::string &text) {
  int n = text.size();
  Rcpp::CharacterVector result(n - 1);
  
  for (int i = 0; i < n - 1; i++) {
    result[i] = text.substr(i, 2);
  }
  return result;
}
