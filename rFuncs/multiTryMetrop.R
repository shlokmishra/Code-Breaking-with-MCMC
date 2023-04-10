source("rFuncs/metropFuncs.R")
N <- 50
minBound <- 1e-8
maxBound <- 1 - 1e-8 

samplingMultiTry <- function(givenCipher, N){
  neighbourSubset_1 <- sample(2:326, size = N)
  score_1 <- numeric(length = N)
  score_2 <- numeric(length = (N-1))
  
  for(k in 1:N){
    i <- allProposal[neighbourSubset_1[k], 1]
    j <- allProposal[neighbourSubset_1[k], 2]
    tempCipher <- swapIndicies(givenCipher,i,j)
    score_1[k] <-  logLik(decodeText(cipheredText, tempCipher))
    for(i in 1:N){
      check <- score_1[i]
      if (check < minBound){
        finalWeight <- minBound
      }
      else if(check > maxBound){
        finalWeight <- maxBound
      }
      else{
        finalWeight <- check
      }
      score_1[i] <- finalWeight
    }
  }
  
  proposal <- sample(neighbourSubset_1, size = 1, prob = exp(score_1)/sum(exp(score_1)))
  indicies_to_swap <- allProposal[proposal, 1:2]
  propCipher <- swapIndicies(givenCipher, indicies_to_swap[1], indicies_to_swap[2])
  neighbourSubset_2 <- sample(2:326, size = N-1)
  
  for(k in 1:(N-1)){
    i <- allProposal[neighbourSubset_2[k], 1]
    j <- allProposal[neighbourSubset_2[k], 2]
    tempCipher <- swapIndicies(propCipher,i,j)
    score_2[k] <-  logLik(decodeText(cipheredText, tempCipher))
    for(i in 1:(N-1)){
      check <- score_2[i]
      if (check < minBound){
        finalWeight <- minBound
      }
      else if(check > maxBound){
        finalWeight <- maxBound
      }
      else{
        finalWeight <- check
      }
      score_2[i] <- finalWeight
    }
  }
  propDecoded <- decodeText(cipheredText,
                            cipher = propCipher)
  currDecoded <- decodeText(cipheredText,
                            cipher = givenCipher)
  
  propLogLik <- logLik(propDecoded)
  currLogLik <- logLik(currDecoded)
  
  num <- sum(exp(score_1))
  den <- sum(exp(score_2)) + exp(logLik(decodeText(cipheredText, givenCipher)))
  
  return(list((propCipher), c(num, den)))
}

decryptMetropMultiTry <- function(cipheredText, N, n){
  currCipher <- startingCipher
  i <- 0
  currDecoded <- decodeText(cipheredText,
                            cipher = currCipher)
  
  currLogLik <- logLik(currDecoded)
  max_score <- currLogLik
  bestCipher <- currCipher
  
  similar <- numeric(length = n)
  similar[1] <- currLogLik
  for (iter in 2:n) {
    
    foo <- samplingMultiTry(currCipher, N)
    propCipher <- foo[[1]]
    num <- foo[[2]][1]
    den <- foo[[2]][2]
    propDecoded <- decodeText(cipheredText,
                              cipher = propCipher)
    currDecoded <- decodeText(cipheredText,
                              cipher = currCipher)
    
    propLogLik <- logLik(propDecoded)
    currLogLik <- logLik(currDecoded)
    
    acceptProb <- min(1, num/den)
    
    accept <- sample(c(TRUE, FALSE),
                     size=1,
                     prob = c(acceptProb,
                              1-acceptProb))
    
    if (accept) {
      currCipher <- propCipher
      currLogLik <- propLogLik
      i <- i + 1
    }
    
    similar[iter] <- currLogLik
    
    if(currLogLik > max_score){
      max_score <- currLogLik
      bestCipher <- currCipher
    }
    if(iter %% (n/100) == 0){
      x <- iter*100/n
      print(paste(x, "% Completed"))
    }
    
    
  }
  bestDecoded <- decodeText(cipheredText,
                            cipher = bestCipher)
  
  return(list(bestDecoded, similar))
}

