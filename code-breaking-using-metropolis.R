  source("function.R")

  
  
  # Choose a value of n which will be the number of iterations
  n <- 50
  
  
  # Choose the string which you would like to encrypt and then decrypt using metropolis algorithm
  plainText <- "let us try to decode the encrypted sentence formed which actually seems to be gibberish"
  
  
  trueCipher <- generateCipher()
  cipheredText <- encodeText(text = plainText,
                               cipher = trueCipher)
  
  
  
  
  decodedBestReg <- decryptMetropReg(cipheredText, n)
  system.time(decoded_text_bestModified <- decryptMetropModified(cipheredText, 1e2))
  
  
  paste("The best decoded text reached in" , n , "iterations is: '")
  decoded_text_bestReg[[1]]
  decoded_text_bestModified[[1]]
  
  
  plot(decoded_text_bestReg[[2]], ylim = range(c(decoded_text_best[[2]], get_log_lik_text(plaintext))),
    xlim = c(1, 1e4), type = 'l')
  lines(decoded_text_bestModified[[2]], col = "green")
  abline(h=get_log_lik_text(plaintext), col = "red")
  
  

