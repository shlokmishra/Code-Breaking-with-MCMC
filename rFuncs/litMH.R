# source("rFuncs/metropFuncs.R")
minBound <- 1e-8
maxBound <- 1 - 1e-8 


samplingInformedLit <- function(givenCipher){
  
  allProposal[1,3] <- logLik(decodeText(cipheredText, givenCipher))
  iter = 1
  pi_x <- allProposal[1,3]
  for(iter in 1:326){
    i <- allProposal[iter, 1]
    j <- allProposal[iter, 2]
    tempCipher <- swapIndicies(givenCipher,i,j)
    pi_y <-  logLik(decodeText(cipheredText, tempCipher))
    check <- pi_y/pi_x
    if (check < minBound){
      finalWeight <- minBound
    }
    else if(check > maxBound){
      finalWeight <- maxBound
    }
    else{
      finalWeight <- check
    }
    
    allProposal[iter, 3] <- finalWeight
  }
  
  prob <- exp(allProposal[-1,3])/sum(exp(allProposal[-1,3]))
  new_proposal <- sample(2:326, size = 1, prob, replace = TRUE)
  indicies_to_swap <- allProposal[new_proposal, 1:2]
  propCipher <- swapIndicies(givenCipher, indicies_to_swap[1], indicies_to_swap[2])
  
  
  allProposalsProp[1,3]<- logLik(decodeText(cipheredText, propCipher))
  
  pi_x <- allProposalsProp[1,3]
  for(iter in 1:326){
    i <- allProposalsProp[iter, 1]
    j <- allProposalsProp[iter, 2] 
    tempCipher <- swapIndicies(propCipher,i,j)
    pi_y <-  logLik(decodeText(cipheredText, tempCipher))
    check <- pi_y/pi_x
    if (check < minBound){
      finalWeight <- minBound
    }
    else if(check > maxBound){
      finalWeight <- maxBound
    }
    else{
      finalWeight <- check
    }
    
    # allProposal[iter, 3] <- finalWeight
    allProposalsProp[iter, 3] <- finalWeight #logLik(decodeText(cipheredText, tempCipher))
  }
  
  q1 <- exp(allProposalsProp[new_proposal,3])/sum(exp(allProposalsProp[,3]))
  q2 <- exp(allProposal[new_proposal, 3])/ sum(exp(allProposal[,3]))
  
  return(list((propCipher), c(q1, q2)))
}

decryptMetropLit <- function(cipheredText, n){
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
    
    foo <- samplingInformedLit(currCipher)
    propCipher <- foo[[1]]
    
    q1 <- foo[[2]][1]
    q2 <- foo[[2]][2]
    
    propDecoded <- decodeText(cipheredText,
                              cipher = propCipher)
    currDecoded <- decodeText(cipheredText,
                              cipher = currCipher)
    
    propLogLik <- logLik(propDecoded)
    currLogLik <- logLik(currDecoded)
    
    
    acceptProb <- min( 1, exp(propLogLik-currLogLik+log(q1)-log(q2)))
    
    
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
  
  return(list(bestDecoded, similar, i/n))
  
}
