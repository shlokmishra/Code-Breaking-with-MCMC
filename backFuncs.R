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


generate_cipher <- function() sample(letters, replace = FALSE)
encode_text <- function(text, cipher){
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


swap_random <- function(x){
  rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
  element_1 <- x[rand_indices[1]]
  element_2 <- x[rand_indices[2]]
  
  x[rand_indices[1]] <- element_2
  x[rand_indices[2]] <- element_1
  
  return(x)
}
swap_given_indicies <- function(given_cipher, i, j){
  temp_cipher <- given_cipher
  element_1 <- temp_cipher[i]
  element_2 <- temp_cipher[j]
  
  temp_cipher[i] <- element_2
  temp_cipher[j] <- element_1
  return(temp_cipher)
}
