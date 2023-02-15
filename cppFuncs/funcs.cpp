#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector get_prob_two_char(CharacterVector text, NumericVector probability_table, int table_size) {
  int text_size = text.size();
  NumericVector prob(text_size);
  for(int i = 0; i < text_size; ++i){
    prob[i] = probability_table[as<std::string>(text[i])];
    if (NumericVector::is_na(prob[i])) {
      prob[i] = 0.5 / table_size;
    }
  }
  return prob;
}

// [[Rcpp::export]]
double logLik(CharacterVector text, NumericVector probability_table, int table_size) {
  CharacterVector two_chars(text.size() - 1);
  for(int i = 1; i < text.size(); ++i){
    two_chars[i-1] = as<std::string>(text[i-1]) + as<std::string>(text[i]);
  }
  NumericVector prob = get_prob_two_char(two_chars, probability_table, table_size);
  double log_lik = sum(log(prob));
  return log_lik;
}
