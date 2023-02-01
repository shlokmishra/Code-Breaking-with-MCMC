library(purrr)
library(Rcpp)
library(doParallel)
library(dplyr)
library(magrittr)
source("backFuncs.R")
sourceCpp("all-possible-proposals.cpp")
all_possible_proposals <- compute_all_scores()
all_possible_proposals[1,1:2] <- c(1,1)
all_possible_proposals_of_proposal <- all_possible_proposals
# all_possible_proposals_of_proposal[1,1:2] <- c(1,1)
# no_cores <- detectCores() - 2
# cl <- makeCluster(no_cores)
# registerDoParallel(cores = no_cores)
result <- c()
#### Generating proposals ###


new_sampling_way <- function(given_cipher){
  
  all_possible_proposals[1,1:2] <- c(1,1)
  all_possible_proposals[1,3]<- get_log_lik_text(decode_text(ciphered_text, given_cipher))
  iter = 1
  
  for(iter in 1:326){
    all_possible_proposals[iter, 1] -> i 
    all_possible_proposals[iter, 2] -> j
    temp_cipher <- swap_given_indicies(given_cipher,i,j)
    all_possible_proposals[iter, 3] <- get_log_lik_text(decode_text(ciphered_text, temp_cipher))
  }

  all_possible_proposals
  all_scores <- as.numeric(all_possible_proposals[,3])
  prob <- exp(all_scores)/sum(exp(all_scores))
  new_proposal <- sample(1:326, size = 1, prob, replace = TRUE)
  indicies_to_swap <- all_possible_proposals[new_proposal, 1:2]
  proposed_cipher <- swap_given_indicies(given_cipher, indicies_to_swap[1], indicies_to_swap[2])
  return(proposed_cipher)
}
new_sampling_wayModified <- function(given_cipher){
  
  # all_possible_proposals[1,1:2] <- c(1,1)
  all_possible_proposals[1,3] <- get_log_lik_text(decode_text(ciphered_text, given_cipher))
  iter = 1
  
  for(iter in 1:326){
    i <- all_possible_proposals[iter, 1]
    j <- all_possible_proposals[iter, 2]
    temp_cipher <- swap_given_indicies(given_cipher,i,j)
    all_possible_proposals[iter, 3] <- get_log_lik_text(decode_text(ciphered_text, temp_cipher))
  }
  # result <- c()
  # result <- foreach(i=1:25) %:% foreach(j=(i+1):26) %dopar% {
  #   temp_cipher <- swap_given_indicies(given_cipher,i,j)
  #   get_log_lik_text(decode_text(ciphered_text, temp_cipher))
  # }
  # length(result)
  # unlist(result)
  # all_possible_proposals[2:326,3] <- unlist(result) 
  # all_possible_proposals
  
  
  
  
  # all_scores <- as.numeric(all_possible_proposals[,3])
  prob <- exp(all_possible_proposals[-1,3])/sum(exp(all_possible_proposals[-1,3]))
  new_proposal <- sample(2:326, size = 1, prob, replace = TRUE)
  indicies_to_swap <- all_possible_proposals[new_proposal, 1:2]
  proposed_cipher <- swap_given_indicies(given_cipher, indicies_to_swap[1], indicies_to_swap[2])
  
  
  all_possible_proposals_of_proposal[1,3]<- get_log_lik_text(decode_text(ciphered_text, proposed_cipher))
  
  
  for(iter in 1:326){
    all_possible_proposals_of_proposal[iter, 1] -> i 
    all_possible_proposals_of_proposal[iter, 2] -> j
    temp_cipher <- swap_given_indicies(proposed_cipher,i,j)
    all_possible_proposals_of_proposal[iter, 3] <- get_log_lik_text(decode_text(ciphered_text, temp_cipher))
  }
  # result <- c()
  # result <- foreach(i=1:25) %:% foreach(j=(i+1):26) %dopar% {
  #   temp_cipher <- swap_given_indicies(proposed_cipher,i,j)
  #   get_log_lik_text(decode_text(ciphered_text, temp_cipher))
  # }
  # length(result)
  # unlist(result)
  # all_possible_proposals_of_proposal[2:326,3] <- unlist(result) 
  # all_possible_proposals
  # all_scores_new <- as.numeric(all_possible_proposals_of_proposal[,3])
  q1 <- exp(all_possible_proposals_of_proposal[1,3])/sum(exp(all_possible_proposals_of_proposal[,3]))
  q2 <- exp(all_possible_proposals[new_proposal, 3])/ sum(exp(all_possible_proposals[,3]))
  # exp(all_possible_proposals[new_proposal, 3])
  return(list((proposed_cipher), c(q1, q2)))
}










### FUNCTIONS DEPLOYING METROPOLIS ALGORITHM TO DERYPT A GIVRN TEXT IN GIVEN N ITERNATIONS ###

decrypt_metrop <- function(ciphered_text, n){
  
  
  current_cipher <- generate_cipher()
  i <- 0
  decoded_text_current <- decode_text(ciphered_text,
                                      cipher = current_cipher)
  
  current_log_lik <- get_log_lik_text(decoded_text_current)
  max_score <- current_log_lik
  best_cipher <- current_cipher
  
  similar <- numeric(length = n)
  similar[1] <- current_log_lik
  for (iter in 2:n) {
    
    proposed_cipher <- new_sampling_way(current_cipher)
    
    decoded_text_proposed <- decode_text(ciphered_text,
                                         cipher = proposed_cipher)
    decoded_text_current <- decode_text(ciphered_text,
                                        cipher = current_cipher)
    
    proposed_log_lik <- get_log_lik_text(decoded_text_proposed)
    current_log_lik <- get_log_lik_text(decoded_text_current)
    
    acceptance_probability <- min(1, exp(proposed_log_lik - current_log_lik))
    
    accept <- sample(c(TRUE, FALSE),
                     size=1,
                     prob = c(acceptance_probability,
                              1-acceptance_probability))
    
    if (accept) {
      current_cipher <- proposed_cipher
      current_log_lik <- proposed_log_lik
      # print(glue::glue("Iteration {i}: {decoded_text_proposed}"))
      i <- i + 1
    }
    
    similar[iter] <- current_log_lik
    
    if(current_log_lik > max_score){
      max_score <- current_log_lik
      best_cipher <- current_cipher
    }
    if(iter %% (n/10) == 0){
      x <- iter*100/n
      print(paste(x, "% Completed"))
    }
    
  }
  decoded_text_best <- decode_text(ciphered_text,
                                   cipher = best_cipher)
  
  print(i/n)
  
  return(list(decoded_text_best, similar))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
  
}



decrypt_metropReg <- function(ciphered_text, n){
  current_cipher <- generate_cipher()
  i <- 0
  decoded_text_current <- decode_text(ciphered_text,
                                      cipher = current_cipher)
  
  current_log_lik <- get_log_lik_text(decoded_text_current)
  max_score <- current_log_lik
  best_cipher <- current_cipher
  
  similar <- numeric(length = n)
  similar[1] <- current_log_lik
  for (iter in 2:n) {
    
    proposed_cipher <- swap_random(current_cipher)
    
    decoded_text_proposed <- decode_text(ciphered_text,
                                         cipher = proposed_cipher)
    decoded_text_current <- decode_text(ciphered_text,
                                        cipher = current_cipher)
    
    proposed_log_lik <- get_log_lik_text(decoded_text_proposed)
    current_log_lik <- get_log_lik_text(decoded_text_current)
    
    acceptance_probability <- min(1, exp(proposed_log_lik - current_log_lik))
    
    accept <- sample(c(TRUE, FALSE),
                     size=1,
                     prob = c(acceptance_probability,
                              1-acceptance_probability))
    
    if (accept) {
      current_cipher <- proposed_cipher
      current_log_lik <- proposed_log_lik
      # print(glue::glue("Iteration {i}: {decoded_text_proposed}"))
      i <- i + 1
    }
    
    similar[iter] <- current_log_lik
    
    if(current_log_lik > max_score){
      max_score <- current_log_lik
      best_cipher <- current_cipher
    }
    if(iter %% (n/10) == 0){
      x <- iter*100/n
      print(paste(x, "% Completed"))
    }
    
    
  }
  decoded_text_best <- decode_text(ciphered_text,
                                   cipher = best_cipher)
  
  print(i/n)
  
  return(list(decoded_text_best, similar))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
}



decrypt_metropModified <- function(ciphered_text, n){
  current_cipher <- generate_cipher()
  i <- 0
  decoded_text_current <- decode_text(ciphered_text,
                                      cipher = current_cipher)
  
  current_log_lik <- get_log_lik_text(decoded_text_current)
  max_score <- current_log_lik
  best_cipher <- current_cipher
  
  similar <- numeric(length = n)
  similar[1] <- current_log_lik
  for (iter in 2:n) {
    
    foo <- new_sampling_wayModified(current_cipher)
    proposed_cipher <- foo[[1]]

    (q1 <- foo[[2]][1])
    (q2 <- foo[[2]][2])

    decoded_text_proposed <- decode_text(ciphered_text,
                                         cipher = proposed_cipher)
    decoded_text_current <- decode_text(ciphered_text,
                                        cipher = current_cipher)
    
    proposed_log_lik <- get_log_lik_text(decoded_text_proposed)
    current_log_lik <- get_log_lik_text(decoded_text_current)
    
  
    acceptance_probability <- min(1,(exp(proposed_log_lik - current_log_lik))*q1/q2)
    
    
    accept <- sample(c(TRUE, FALSE),
                     size=1,
                     prob = c(acceptance_probability,
                              1-acceptance_probability))
    
    if (accept) {
      current_cipher <- proposed_cipher
      current_log_lik <- proposed_log_lik
      # print(glue::glue("Iteration {i}: {decoded_text_proposed}"))
      i <- i + 1
    }
    
    similar[iter] <- current_log_lik
    
    if(current_log_lik > max_score){
      max_score <- current_log_lik
      best_cipher <- current_cipher
    }
    if(iter %% (n/10) == 0){
      x <- iter*100/n
      print(paste(x, "% Completed"))
    }
    
  }
  decoded_text_best <- decode_text(ciphered_text,
                                   cipher = best_cipher)
  
  print(i/n)
  
  return(list(decoded_text_best, similar))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
  
}




