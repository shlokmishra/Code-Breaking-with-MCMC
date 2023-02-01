source("function.R")

plaintext <- "the quick brown fox jumps over the lazy dog there are eight billion people in this world"
true_cipher <- generate_cipher()
ciphered_text <- encode_text(text = plaintext,
                             cipher = true_cipher)



given_cipher <- generate_cipher()

all_possible_proposals <- compute_all_scores()
all_possible_proposals[1,1:2] <- c(1,1)
all_possible_proposals_of_proposal <- all_possible_proposals
# all_possible_proposals_of_proposal[1,1:2] <- c(1,1)


new_sampling_wayModified <- function(given_cipher){
  
  # all_possible_proposals[1,1:2] <- c(1,1)
  all_possible_proposals[1,3] <- get_log_lik_text(decode_text(ciphered_text, given_cipher))
  iter = 1
  
  for(iter in 1:326){
        all_possible_proposals[iter, 1] -> i 
        all_possible_proposals[iter, 2] -> j
        temp_cipher <- swap_given_indicies(given_cipher,i,j)
        all_possible_proposals[iter, 3] <- get_log_lik_text(decode_text(ciphered_text, temp_cipher))
  }
  
  
  # all_scores <- as.numeric(all_possible_proposals[,3])
  prob <- exp(all_possible_proposals[,3])/sum(exp(all_possible_proposals[,3]))
  new_proposal <- sample(1:326, size = 1, prob, replace = TRUE)
  indicies_to_swap <- all_possible_proposals[new_proposal, 1:2]
  proposed_cipher <- swap_given_indicies(given_cipher, indicies_to_swap[1], indicies_to_swap[2])
  
  
  all_possible_proposals_of_proposal[1,3]<- get_log_lik_text(decode_text(ciphered_text, proposed_cipher))
  
  
  for(iter in 1:326){
    all_possible_proposals_of_proposal[iter, 1] -> i 
    all_possible_proposals_of_proposal[iter, 2] -> j
    temp_cipher <- swap_given_indicies(proposed_cipher,i,j)
    all_possible_proposals[iter, 3] <- get_log_lik_text(decode_text(ciphered_text, temp_cipher))
  }
  # all_scores_new <- as.numeric(all_possible_proposals_of_proposal[,3])
  q1 <- (all_possible_proposals[1,3])/sum(exp(all_possible_proposals_of_proposal[,3]))
  q2 <- exp(all_possible_proposals[new_proposal, 3])/ sum(exp(all_possible_proposals[,3]))
  # exp(all_possible_proposals[new_proposal, 3])
  return(list((proposed_cipher), c(q1, q2)))
}


result <- c()  

































