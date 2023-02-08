  source("metropFuncs.R")

  
  
  # Choose a value of n which will be the number of iterations
  n <- 50000
  
  
  # Choose the string which you would like to encrypt and then decrypt using metropolis algorithm
  plainText <- "prince wished to obtain this post for his son but others were trying through dowager"
  
  
  # Generation of a random cipher to encrypt the given text 
  trueCipher <- generateCipher()
  cipheredText <- encodeText(text = plainText,
                               cipher = trueCipher)
  
  
  
  # Using both Regular and Modified Metropolis Algorithm functions to decrypt 
  # the given text in the given number of iterations
  decodedBestReg <- decryptMetropReg(cipheredText, n)
  decodedBestModified <- decryptMetropModified(cipheredText, n)
  
  # Printing the decrypted text which has highest similarity score
  paste("The best decoded text reached in" , n , "iterations is: '")
  decodedBestReg[[1]]
  decodedBestModified[[1]]
  
  # Visualising the change in similarity score throughout the process
  plot(decodedBestReg[[2]], ylim = range(c(decodedBestReg[[2]], logLik(plainText))),
    xlim = c(1, 1e5/2), type = 'l')
  lines(decodedBestModified[[2]], col = "green")
  abline(h=logLik(plainText), col = "red")
  
# save(decodedBestModified, decodedBestReg, file = "data50kIter.Rdata")  
  

