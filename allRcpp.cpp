#include <Rcpp.h>
using namespace Rcpp;
Rcpp::Environment env = Rcpp::Environment::global_env();
Rcpp::Function load("load");
load("data.Rdata", env);

// Access war_and_peace_2_characters and probability_table from the R environment
Rcpp::CharacterVector war_and_peace_2_characters = env["war_and_peace_2_characters"];
Rcpp::NumericVector probability_table = env["probability_table"];

// Define the get_prob_two_char function
double get_prob_two_char(std::string two_char) {
  NumericVector prob_from_table = as<NumericVector>(probability_table(two_char));
  
  if (R_IsNA(prob_from_table[0])) {
    return 0.5 / war_and_peace_2_characters.length();
  } else {
    return prob_from_table[0];
  }
}

// Define the break_into_two_chars function
CharacterVector break_into_two_chars(std::string text) {
  int n = text.length();
  IntegerVector starting_indices = seq(0, n - 2);
  CharacterVector out(n - 1);
  
  for (int i = 0; i < n - 1; ++i) {
    out[i] = text.substr(starting_indices[i], 2);
  }
  
  return out;
}

// Define the get_log_lik_text function
double get_log_lik_text(std::string text) {
  CharacterVector two_chars = break_into_two_chars(text);
  int n = two_chars.length();
  double log_lik = 0;
  
  for (int i = 0; i < n; ++i) {
    log_lik += log(get_prob_two_char(as<std::string>(two_chars[i])));
  }
  
  return log_lik;
}

// Define the Rcpp functions to be exported
RcppExport SEXP Rcpp_get_log_lik_text(SEXP text) {
  return wrap(get_log_lik_text(as<std::string>(text)));
}

RcppExport SEXP Rcpp_break_into_two_chars(SEXP text) {
  return wrap(break_into_two_chars(as<std::string>(text)));
}

RcppExport SEXP Rcpp_get_prob_two_char(SEXP two_char) {
  return wrap(get_prob_two_char(as<std::string>(two_char)));
}
