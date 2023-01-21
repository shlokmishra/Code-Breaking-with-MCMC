all_possible_proposals <- matrix(NA,nrow = 326 ,ncol = 3)
all_possible_proposals[1,1:2] <- c(1,1)
all_possible_proposals[1,3]<- get_log_lik_text(decode_text(ciphered_text, current_cipher))
iter = 1
for( i in 1:25){
  for(j in (i+1):26){
    iter = iter + 1
    all_possible_proposals[iter, 1] <- i 
    all_possible_proposals[iter, 2] <- j 
    temp_cipher <- swap_given_indicies(current_cipher,i,j)
    all_possible_proposals[iter, 3] <- get_log_lik_text(decode_text(ciphered_text, temp_cipher))
  } 
}
all_possible_proposals
all_scores <- as.numeric(all_possible_proposals[,3])
# all_scores
prob <- exp(all_scores)/sum(exp(all_scores))
# prob <- exp(all_scores)
# plot(prob)

# prob
new_proposal <- sample(1:326, size = 1, prob, replace = TRUE)
indicies_to_swap <- all_possible_proposals[new_proposal, 1:2]
proposed_cipher <- swap_given_indicies(current_cipher, indicies_to_swap[1], indicies_to_swap[2])
