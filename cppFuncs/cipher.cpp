#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
long double get_prob_two_charCPP(std::string two_char) {
  long double prob_from_table;
  
  Environment global = Environment::global_env();
  CharacterVector war_and_peace_2_characters = global["war_and_peace_2_characters"];
  NumericVector probability_table = global["probability_table"];
  
  // prob_from_table = probability_table[std::find(war_and_peace_2_characters.begin(), war_and_peace_2_characters.end(), two_char) - war_and_peace_2_characters.begin()];
  prob_from_table = probability_table[std::find(war_and_peace_2_characters.begin(), war_and_peace_2_characters.end(), two_char)];
  
  if (NumericVector::is_na(prob_from_table)) {
    return 0.5 / war_and_peace_2_characters.size();
  } else {
    return prob_from_table;
  }
}
