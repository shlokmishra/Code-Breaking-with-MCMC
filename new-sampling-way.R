source("function.R")
plaintext <- "the quick brown fox jumps over the lazy dog there are eight billion people in this world"
true_cipher <- generate_cipher()
ciphered_text <- encode_text(text = plaintext,
                             cipher = true_cipher)
current_cipher <- generate_cipher()
total_possibilities <- choose(26,2)

new_sampling_way <- function(given_cipher){
  all_possible_proposals <- matrix(NA,nrow = 326 ,ncol = 27)
  
  all_possible_proposals[1, 1:26] <- given_cipher
  all_possible_proposals[1, 27] <- get_log_lik_text(decode_text(ciphered_text, given_cipher))
  # all_possible_proposals[1,] 
  
  iter = 1
  for( i in 1:25){
    for(j in (i+1):26){
      iter = iter + 1
      temp_cipher <- given_cipher
      element_1 <- temp_cipher[i]
      element_2 <- temp_cipher[j]
      
      temp_cipher[i] <- element_2
      temp_cipher[j] <- element_1
      
      all_possible_proposals[iter, 1:26] <- temp_cipher 
      all_possible_proposals[iter, 27] <- get_log_lik_text(decode_text(ciphered_text, temp_cipher))
    } 
  }
  all_scores <- as.numeric(all_possible_proposals[,27])
  prob <- all_scores/sum(all_scores)
  
  new_proposal <- sample(1:326, size = 1, prob, replace = TRUE)
  proposed_cipher <- all_possible_proposals[new_proposal, 1:26]
}



