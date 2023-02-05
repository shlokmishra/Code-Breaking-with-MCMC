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

// [[Rcpp::export]]
long double get_prob_two_charCPP(std::string two_char) {
  long double prob_from_table;
  
  Environment global = Environment::global_env();
  CharacterVector war_and_peace_2_characters = global["war_and_peace_2_characters"];
  NumericVector probability_table = global["probability_table"];
  
  prob_from_table = probability_table[std::find(war_and_peace_2_characters.begin(), war_and_peace_2_characters.end(), two_char) - war_and_peace_2_characters.begin()];
  
  if (NumericVector::is_na(prob_from_table)) {
    return 0.5 / war_and_peace_2_characters.size();
  } else {
    return prob_from_table;
  }
}


// [[Rcpp::export]]
long double get_log_likCPP(Rcpp::CharacterVector text){
  
  Rcpp::CharacterVector two_char_text;
  int n = text.size();
  Rcpp::CharacterVector result(n - 1);
  
  for (int i = 0; i < n - 1; i++) {
    std::string substr = Rcpp::as<std::string>(text[i]) + Rcpp::as<std::string>(text[i + 1]);
    result[i] = substr;
  }
  two_char_text = result;
  
  
  
  
  Rcpp::NumericVector log_prob(two_char_text.length());
  for(int i = 0; i < two_char_text.length(); ++i){
    log_prob[i] = log(get_prob_two_charCPP(Rcpp::as<std::string>(two_char_text[i])));
  }
  return Rcpp::sum(log_prob);
}
