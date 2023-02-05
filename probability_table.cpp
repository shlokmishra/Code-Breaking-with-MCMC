#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix create_probability_table(const CharacterVector &text) {
  int n = text.size();
  NumericMatrix result(n, n, 0.0);
  
  for (int i = 0; i < n - 1; i++) {
    std::string current_char = Rcpp::as<std::string>(text[i]);
    std::string next_char = Rcpp::as<std::string>(text[i + 1]);
    int current_char_index = -1;
    int next_char_index = -1;
    
    // Find the indices of the current and next characters in the probability table
    for (int j = 0; j < n; j++) {
      if (current_char == Rcpp::as<std::string>(text[j])) {
        current_char_index = j;
      }
      if (next_char == Rcpp::as<std::string>(text[j])) {
        next_char_index = j;
      }
    }
    
    // Increment the count of the current character followed by the next character
    result(current_char_index, next_char_index) += 1.0;
  }
  
  // Normalize the counts to get probabilities
  for (int i = 0; i < n; i++) {
    double sum = 0.0;
    for (int j = 0; j < n; j++) {
      sum += result(i, j);
    }
    for (int j = 0; j < n; j++) {
      result(i, j) /= sum;
    }
  }
  
  return result;
}
