source("rFuncs/metropFuncs.R")
n <- 50

neighbourSubset <- sample(1:326, size = n)
score_1 <- numeric(length = n)
score_2 <- numeric(length = n-1)

for(k in 1:n){
  i <- allProposal[neighbourSubset[k], 1]
  j <- allProposal[neighbourSubset[k], 2]
  tempCipher <- swapIndicies(givenCipher,i,j)
  score_1[k] <-  logLik(decodeText(cipheredText, tempCipher))
}

