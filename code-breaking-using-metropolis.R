library(profvis)
# install.packages("profvis")

# profvis({
  source("function.R")
  # Choose a value of n which will be the number of iterations
  n <- 500
  
  
  # Choose the string which you would like to encrypt and then decrypt using metropolis algorithm
  plaintext <- "the quick brown fox jumps over the lazy dog there are eight billion people in this world"
  
  
  true_cipher <- generate_cipher()
  ciphered_text <- encode_text(text = plaintext,
                               cipher = true_cipher)
  
  
  
  
  # decoded_text_best <- decrypt_metrop(ciphered_text, n)
  # decoded_text_bestReg <- decrypt_metropReg(ciphered_text, n)
  decoded_text_bestModified <- decrypt_metropModified(ciphered_text, n)
  paste("The best decoded text reached in" , n , "iterations is: '")
  decoded_text_best[[1]]
  decoded_text_bestReg[[1]]
  decoded_text_bestModified[[1]]
  plot.ts(decoded_text_best[[2]], ylim = range(c(decoded_text_best[[2]], get_log_lik_text(plaintext))))
  lines(decoded_text_bestReg[[2]], col = "blue")
  lines(decoded_text_bestModified[[2]], col = "green")
  abline(h=get_log_lik_text(plaintext), col = "red")
  
  
# })
