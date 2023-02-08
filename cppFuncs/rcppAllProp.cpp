#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]

NumericMatrix compute_all_scores(){
  // import("function.R");
  // Function f1("get_log_lik_text");
  // Function f2("decode_text");
  // Function f3("swap_given_indicies");
  
  int iter = 0;
  NumericMatrix mat(326,3);
  for(int i=1; i<=25; i++){
    for(int j=(i+1); j<=26; j++){
      iter++;
      mat(iter,0) = i;
      mat(iter,1) = j;
      // CharacterVector temp_cipher = f3(given_cipher, i, j);
      // mat(iter,3) = f1(f2(ciphered_text, temp_cipher));
    }
  }
  
  return mat;
}

// [[Rcpp::export]]

CharacterVector swap_given_indicies_cpp(CharacterVector given_cipher, int i, int j){
  CharacterVector proposal_cipher = given_cipher;
  String element_1 = given_cipher[i];
  String element_2 = given_cipher[j];
  
  proposal_cipher[i] = element_2; 
  proposal_cipher[j] = element_1; 
  
  return proposal_cipher;
}



// NumericVector calc_log_lik(CharacterVector ){}


// CharacterVector break_into_two_chars_cpp(CharacterVector text){
//   CharacterVector str;
//   IntegerVector starting_indicies = 
//   IntegerVector ending_indicies = 
//   
//   return str;
// }









