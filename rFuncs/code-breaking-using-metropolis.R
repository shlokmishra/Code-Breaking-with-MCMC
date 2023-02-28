  source("rFuncs/metropFuncs.R")
  # source("rFuncs/new-prop-dist.R")
  
  
  # Choose a value of n which will be the number of iterations
  n <- 100
  
  
  # Choose the string which you would like to encrypt and then decrypt using metropolis algorithm
  plainText <- "prince wished to obtain this post for his son but others were trying through dowager"
  
  
  # Generation of a random cipher to encrypt the given text 
  trueCipher <- generateCipher()
  cipheredText <- encodeText(text = plainText,
                               cipher = trueCipher)
  
  
  
  # Using both Regular and Modified Metropolis Algorithm functions to decrypt 
  # the given text in the given number of iterations
  decodedBestReg <- decryptMetropReg(cipheredText, n)
  decodedBestBarker <- decryptMetropBarker(cipheredText, n)
  decodedBestLit <- decryptMetropLit(cipheredText, n)
  
  # Printing the decrypted text which has highest similarity score
  paste("The best decoded text reached in" , n , "iterations is: '")
  decodedBestReg[[1]]
  decodedBestBarker[[1]]
  decodedBestLit[[1]]
  
  # Visualising the change in similarity score throughout the process
  plot(decodedBestReg[[2]], ylim = range(c(decodedBestReg[[2]], logLik(plainText))),
    xlim = c(1, n), type = 'l')
  lines(decodedBestBarker[[2]], col = "red")
  lines(decodedBestLit[[2]], col = "blue")
  
  abline(h=logLik(plainText), col = "green")
  legend("bottomright", legend = c("Reg", "Barker", "Lit"),lty = c(1,1,1) , col = c("black", "red", "blue")) 
  # save(decodedBestModified, decodedBestReg, n, plainText, file = "data50kIter.Rdata")
    


