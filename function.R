library(purrr)

encrypt_decrypt_using_metropolis <- function(ciphered_text, n){
  
  #plaintext <- "this is a sample peice of text and i am really glad that this code can easily crack it"
  
  # Generate a new cipher by permuting the letters of the alphabet
  # generate_cipher <- function() sample(letters,
  #                                      replace = FALSE)
  # 
  # # Encode a text using a cipher
  # encode_text <- function(text, cipher) {
  #   chartr(
  #     x = text,
  #     old = paste(letters, collapse = ""),
  #     new = paste(cipher, collapse = "")
  #   )
  # }
  # 
  # # Decode a text given a cipher
  decode_text <- function(ciphered_text, cipher) {
    chartr(
      x = ciphered_text,
      old = paste(cipher, collapse = ""),
      new = paste(letters, collapse = "")
    )
  }
  
  
  current_cipher <- generate_cipher()
  
  swap <- function(x){
    # Select two distinct indices
    rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
    element_1 <- x[rand_indices[1]]
    element_2 <- x[rand_indices[2]]
    
    x[rand_indices[1]] <- element_2
    x[rand_indices[2]] <- element_1
    
    return(x)
  }
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
      return(1 / length(war_and_peace_2_characters))
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
  # war_and_peace <- readr::read_file("https://www.gutenberg.org/cache/epub/2600/pg2600.txt")
  # war_and_peace <-
  #   war_and_peace |>
  #   stringr::str_to_lower() |>
  #   gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |>
  #   stringi::stri_trans_general(id = "Latin-ASCII")
  # war_and_peace_2_characters <- break_into_two_chars(war_and_peace)
  # Ten most common two-character combinations
  
  load("war_and_peace_2_characters.Rdata")
  probability_table <-
    table(war_and_peace_2_characters) / length(war_and_peace_2_characters)
  
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
  }
  decoded_text_best <- decode_text(ciphered_text,
                                   cipher = current_cipher)
  print(i/n)
  
  return(list(decoded_text_best, similar))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
  
}



