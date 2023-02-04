#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
// Retrieving the global environment
Environment env = Environment::global_env();

// Retrieving object "x" from the global environment
NumericVector x = env["x"];
void rcpp_rprintf(NumericVector v){
  // printing values of all the elements of Rcpp vector  
  for(int i=0; i<v.length(); ++i){
    Rprintf("the value of v[%i] : %f \n", i, v[i]);
  }
}

// Rcpp::Environment env = Rcpp::Environment::global_env();
// Rcpp::Function load("load");
// load("data.Rdata", envir = env);





// // [[Rcpp::export]]
// Rcpp::CharacterVector break_into_two_charsCpp(const std::string &text) {
//   int n = text.size();
//   Rcpp::CharacterVector result(n - 1);
//   
//   for (int i = 0; i < n - 1; i++) {
//     result[i] = text.substr(i, 2);
//   }
//   
//   return result;
// }
// 
// 
// // [[Rcpp::export]]
// Rcpp::NumericVector map_dblCPP(Rcpp::Function func, Rcpp::List list) {
//   int n = list.size();
//   Rcpp::NumericVector result(n);
//   
//   for (int i = 0; i < n; i++) {
//     result[i] = Rcpp::as<double>(func(list[i]));
//   }
//   
//   return result;
// }
// 
// 
// 
// // [[Rcpp::export]]
// double get_prob_two_charCPP(const std::string &two_char,
//                          const Rcpp::CharacterVector &probability_table,
//                          const Rcpp::CharacterVector &war_and_peace_2_characters) {
//   
//   int index = std::find(war_and_peace_2_characters.begin(), 
//                         war_and_peace_2_characters.end(), two_char) - 
//                           war_and_peace_2_characters.begin();
//   
//   if (index == war_and_peace_2_characters.size()) {
//     return 0.5 / war_and_peace_2_characters.size();
//   }
//   
//   double prob_from_table = Rcpp::as<double>(probability_table[index]);
//   
//   if (Rcpp::NumericVector::is_na(prob_from_table)) {
//     return 0.5 / war_and_peace_2_characters.size();
//   } else {
//     return prob_from_table;
//   }
// }
