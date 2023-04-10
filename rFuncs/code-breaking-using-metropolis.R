  source("rFuncs/metropFuncs.R")
  source("rFuncs/litMH.R")
  source("rFuncs/multiTryMetrop.R")

  
  # Choose a value of n which will be the number of iterations
  n <- 1000
  
  
  # Choose the string which you would like to encrypt and then decrypt using metropolis algorithm
  plainText <- "to be or not to be that is the question whether tis nobler in the mind to suffer the slings and arrows of outrageous fortune or to take arms against a sea of troubles"
  
  
  # Generation of a random cipher to encrypt the given text 
  trueCipher <- generateCipher()
  cipheredText <- encodeText(text = plainText,
                               cipher = trueCipher)
  
  
  
  # Using both Regular and Modified Metropolis Algorithm functions to decrypt 
  # the given text in the given number of iterations
  decodedBestReg <- decryptMetropReg(cipheredText, n)
  decodedBestBarker <- decryptMetropBarker(cipheredText, n)
  decodedBestLit <- decryptMetropLit(cipheredText, n)
  decodedBestMultiTry <- decryptMetropMultiTry(cipheredText,50 ,n)
  
  # Printing the decrypted text which has highest similarity score
  paste("The best decoded text reached in" , n , "iterations is: '")
  decodedBestReg[[1]]
  decodedBestBarker[[1]]
  decodedBestLit[[1]]
  decodedBestMultiTry[[1]]
  # Visualising the change in similarity score throughout the process
  plot(decodedBestReg[[2]], ylim = range(c(decodedBestReg[[2]], logLik(plainText))),
    xlim = c(1, n), type = 'l', xlab = "Log-Likelihoods", ylab = "Number of Iterations", main = "Comparing different MH algorithms")
  lines(decodedBestBarker[[2]], col = "red")
  lines(decodedBestLit[[2]], col = "blue")
  lines(decodedBestMultiTry[[2]], col = "purple")
  abline(h=logLik(plainText), col = "gray")
  legend("bottomright", legend = c("RW", "Barker", "LIT", "MultiTry"),lty = c(1,1,1,1) , col = c("black", "red", "blue", "purple")) 
  
  # Uncomment the next line to save your results
  save(decodedBestBarker,decodedBestLit, decodedBestReg,decodedBestMultiTry, n, plainText, file = "data/multi-try-1.Rdata")
    


