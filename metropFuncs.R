###LOADING REQUIRED LIBRARIES AND SOURCING SOME FUNCTIONS & CPP FILES###
library(purrr)
library(Rcpp)
source("cipherFuncs.R")
source("scoreFuncs.R")
sourceCpp("rcppAllProp.cpp")
allProposal <- compute_all_scores()
allProposal[1,1:2] <- c(1,1)
allProposalsProp <- allProposal

### FUNCTION SAMPLING FROM THE NEIGHBOURS OF THE CURRENT CIPHER IN AN INFORMED MANNER###
samplingInformed <- function(givenCipher){
  
  allProposal[1,3] <- logLik(decodeText(cipheredText, givenCipher))
  iter = 1
  
  for(iter in 1:326){
    i <- allProposal[iter, 1]
    j <- allProposal[iter, 2]
    tempCipher <- swapIndicies(givenCipher,i,j)
    allProposal[iter, 3] <- logLik(decodeText(cipheredText, tempCipher))
  }
  
  prob <- exp(allProposal[-1,3])/sum(exp(allProposal[-1,3]))
  new_proposal <- sample(2:326, size = 1, prob, replace = TRUE)
  indicies_to_swap <- allProposal[new_proposal, 1:2]
  propCipher <- swapIndicies(givenCipher, indicies_to_swap[1], indicies_to_swap[2])
  
  
  allProposalsProp[1,3]<- logLik(decodeText(cipheredText, propCipher))
  
  
  for(iter in 1:326){
    allProposalsProp[iter, 1] -> i 
    allProposalsProp[iter, 2] -> j
    tempCipher <- swapIndicies(propCipher,i,j)
    allProposalsProp[iter, 3] <- logLik(decodeText(cipheredText, tempCipher))
  }
  
  q1 <- exp(allProposalsProp[new_proposal,3])/sum(exp(allProposalsProp[,3]))
  q2 <- exp(allProposal[new_proposal, 3])/ sum(exp(allProposal[,3]))
  
  return(list((propCipher), c(q1, q2)))
}


### FUNCTION DEPLOYING METROPOLIS ALGORITHM TO DERYPT A GIVRN TEXT IN GIVEN N ITERNATIONS ###
decryptMetropReg <- function(cipheredText, n){
  currCipher <- generateCipher()
  i <- 0
  currDecoded <- decodeText(cipheredText,
                                      cipher = currCipher)
  
  currLogLik <- logLik(currDecoded)
  max_score <- currLogLik
  bestCipher <- currCipher
  
  similar <- numeric(length = n)
  similar[1] <- currLogLik
  for (iter in 2:n) {
    
    propCipher <- swapRand(currCipher)
    
    propDecoded <- decodeText(cipheredText,
                                         cipher = propCipher)
    currDecoded <- decodeText(cipheredText,
                                        cipher = currCipher)
    
    propLogLik <- logLik(propDecoded)
    currLogLik <- logLik(currDecoded)
    
    acceptProb <- min(1, exp(propLogLik - currLogLik))
    
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

### FUNCTION DEPLOYING METROPOLIS ALGORITHM WITH MUCH INFORMED PROPOSALS TO DERYPT A GIVRN TEXT IN GIVEN N ITERNATIONS ###
decryptMetropModified <- function(cipheredText, n){
  currCipher <- generateCipher()
  i <- 0
  currDecoded <- decodeText(cipheredText,
                                      cipher = currCipher)
  
  currLogLik <- logLik(currDecoded)
  max_score <- currLogLik
  bestCipher <- currCipher
  
  similar <- numeric(length = n)
  similar[1] <- currLogLik
  for (iter in 2:n) {
    
    foo <- samplingInformed(currCipher)
    propCipher <- foo[[1]]

    q1 <- foo[[2]][1]
    q2 <- foo[[2]][2]

    propDecoded <- decodeText(cipheredText,
                                         cipher = propCipher)
    currDecoded <- decodeText(cipheredText,
                                        cipher = currCipher)
    
    propLogLik <- logLik(propDecoded)
    currLogLik <- logLik(currDecoded)
    
  
    acceptProb <- min(1,exp(propLogLik - currLogLik)) #+ log(q1) - log(q2)))
    
    
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




