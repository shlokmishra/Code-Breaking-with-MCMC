library(purrr)

#### FUNCITONS FOR DEALING WITH CIPHERS ###
generate_cipher <- function() sample(letters, replace = FALSE)
encode_text <- function(text, cipher) {
  chartr(
    x = text,
    old = paste(letters, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}
decode_text <- function(ciphered_text, cipher) {
  chartr(
    x = ciphered_text,
    old = paste(cipher, collapse = ""),
    new = paste(letters, collapse = "")
  )
}
swap <- function(x){
  rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
  element_1 <- x[rand_indices[1]]
  element_2 <- x[rand_indices[2]]
  
  x[rand_indices[1]] <- element_2
  x[rand_indices[2]] <- element_1
  
  return(x)
}
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

### USING WAR AND PEACE TO CALCULATE BI-GRAM FREQUENCIES####
load("war_and_peace_2_characters.Rdata")
probability_table <- table(war_and_peace_2_characters) / length(war_and_peace_2_characters)
get_log_lik_text <- function(text){
  text %>%
    break_into_two_chars() %>%
    purrr::map_dbl(get_prob_two_char) %>%
    log() %>%
    sum()
}
get_prob_two_char <- function(two_char){
  prob_from_table <- probability_table[two_char]
  
  if (is.na(prob_from_table)) {
    return(0.5/ length(war_and_peace_2_characters))
  } else{
    return(prob_from_table)
  }
}
break_into_two_chars <- function(text){
  starting_indices <- 1 : (nchar(text) - 1)
  ending_indices <- starting_indices + 1
  return(stringi::stri_sub(text,
                           from = starting_indices,
                           to = ending_indices))
}


encrypt_decrypt_using_metropolis <- function(ciphered_text, n){
  
  
  current_cipher <- generate_cipher()

  # war_and_peace <- readr::read_file("https://www.gutenberg.org/cache/epub/2600/pg2600.txt")
  # war_and_peace <-
  #   war_and_peace |>
  #   stringr::str_to_lower() |>
  #   gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |>
  #   stringi::stri_trans_general(id = "Latin-ASCII")
  # war_and_peace_2_characters <- break_into_two_chars(war_and_peace)
  # Ten most common two-character combinations
  
  
  
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
    if(iter == n/10){
      print(" 10% Completed")
    }
    else if(iter == 2*n/10){
      print(" 20% Completed")
    }
    else if(iter == 3*n/10){
      print(" 30% Completed")
    }
    else if(iter == 4*n/10){
      print(" 40% Completed")
    }
    else if(iter == 5*n/10){
      print(" 50% Completed")
    }
    else if(iter == 6*n/10){
      print(" 60% Completed")
    }
    else if(iter == 7*n/10){
      print(" 70% Completed")
    }
    else if(iter == 8*n/10){
      print(" 80% Completed")
    }
    else if(iter == 9*n/10){
      print(" 90% Completed")
    }
    else if(iter == 10*n/10){
      print(" 100% Completed")
    }
    
  }
  decoded_text_best <- decode_text(ciphered_text,
                                   cipher = current_cipher)
  
  print(i/n)
  
  return(list(decoded_text_best, similar))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
  
}




reg_metropolis <- function(ciphered_text, n){
  
  
  current_cipher <- generate_cipher()

  # war_and_peace <- readr::read_file("https://www.gutenberg.org/cache/epub/2600/pg2600.txt")
  # war_and_peace <-
  #   war_and_peace |>
  #   stringr::str_to_lower() |>
  #   gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |>
  #   stringi::stri_trans_general(id = "Latin-ASCII")
  # war_and_peace_2_characters <- break_into_two_chars(war_and_peace)
  # Ten most common two-character combinations
  
  
  
  i <- 0
  decoded_text_current <- decode_text(ciphered_text,
                                      cipher = current_cipher)
  
  current_log_lik <- get_log_lik_text(decoded_text_current)
  max_score <- current_log_lik
  best_cipher <- current_cipher
  
  similar <- numeric(length = n)
  similar[1] <- current_log_lik
  for (iter in 2:n) {
    
    proposed_cipher <- swap(current_cipher)
    
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
    if(iter == n/10){
      print(" 10% Completed")
    }
    else if(iter == 2*n/10){
      print(" 20% Completed")
    }
    else if(iter == 3*n/10){
      print(" 30% Completed")
    }
    else if(iter == 4*n/10){
      print(" 40% Completed")
    }
    else if(iter == 5*n/10){
      print(" 50% Completed")
    }
    else if(iter == 6*n/10){
      print(" 60% Completed")
    }
    else if(iter == 7*n/10){
      print(" 70% Completed")
    }
    else if(iter == 8*n/10){
      print(" 80% Completed")
    }
    else if(iter == 9*n/10){
      print(" 90% Completed")
    }
    else if(iter == 10*n/10){
      print(" 100% Completed")
    }
    
  }
  decoded_text_best <- decode_text(ciphered_text,
                                   cipher = current_cipher)
  
  print(i/n)
  
  return(list(decoded_text_best, similar))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
  
}



