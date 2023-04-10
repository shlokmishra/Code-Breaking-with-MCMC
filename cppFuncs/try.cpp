#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
StringVector break_into_two_chars(std::string text) {
  int n = text.size() - 1;
  StringVector two_chars(n);
  
  for (int i = 0; i < n; ++i) {
    two_chars[i] = text.substr(i, 2);
  }
  
  return two_chars;
}

// [[Rcpp::export]]
double get_prob_two_charCPP(CharacterVector probability_table, String two_char) {
  CharacterVector::iterator it = std::find(probability_table.begin(), probability_table.end(), two_char);
  
  if (it == probability_table.end()) {
    return 0.5 / probability_table.size();
  } else {
    return it - probability_table.begin();
  }
}

// [[Rcpp::export]]
double logLik(std::string text, CharacterVector probability_table) {
  StringVector two_chars = break_into_two_chars(text);
  double log_sum = 0.0;
  
  for (int i = 0; i < two_chars.size(); ++i) {
    double prob = get_prob_two_charCPP(probability_table, two_chars[i]);
    log_sum += log(prob);
  }
  
  return log_sum;
}
