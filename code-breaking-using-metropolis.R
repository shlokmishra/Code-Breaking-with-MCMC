  source("function.R")

  
  
  # Choose a value of n which will be the number of iterations
  n <- 50
  
  
  # Choose the string which you would like to encrypt and then decrypt using metropolis algorithm
  plainText <- "let us try to decode the encrypted sentence formed which actually seems to be gibberish"
  
  
  trueCipher <- generateCipher()
  cipheredText <- encodeText(text = plainText,
                               cipher = trueCipher)
  
  
  
  
  decodedBestReg <- decryptMetropReg(cipheredText, n)
  decodedBestModified <- decryptMetropModified(cipheredText, 1e2)
  
  
  paste("The best decoded text reached in" , n , "iterations is: '")
  decodedBestReg[[1]]
  decodedBestModified[[1]]
  
  
  plot(decodedBestReg[[2]], ylim = range(c(decodedBestReg[[2]], logLik(plainText))),
    xlim = c(1, 1e3), type = 'l')
  lines(decodedBestModified[[2]], col = "green")
  abline(h=logLik(plainText), col = "red")
  
  

