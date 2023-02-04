library(purrr)
library(Rcpp)
library(dplyr)
library(magrittr)
source("backFuncs.R")
sourceCpp("all-possible-proposals.cpp")
allProposal <- compute_all_scores()
allProposal[1,1:2] <- c(1,1)
allProposalsProp <- allProposal



# new_sampling_way <- function(givenCipher){
#   
#   allProposal[1,1:2] <- c(1,1)
#   allProposal[1,3]<- logLik(decodeText(cipheredText, givenCipher))
#   iter = 1
#   
#   for(iter in 1:326){
#     allProposal[iter, 1] -> i 
#     allProposal[iter, 2] -> j
#     tempCipher <- swapGiven(givenCipher,i,j)
#     allProposal[iter, 3] <- logLik(decodeText(cipheredText, tempCipher))
#   }
# 
#   allProposal
#   all_scores <- as.numeric(allProposal[,3])
#   prob <- exp(all_scores)/sum(exp(all_scores))
#   new_proposal <- sample(1:326, size = 1, prob, replace = TRUE)
#   indicies_to_swap <- allProposal[new_proposal, 1:2]
#   propCipher <- swapGiven(givenCipher, indicies_to_swap[1], indicies_to_swap[2])
#   return(propCipher)
# }
samplingInformed <- function(givenCipher){
  
  # allProposal[1,1:2] <- c(1,1)
  allProposal[1,3] <- logLik(decodeText(cipheredText, givenCipher))
  iter = 1
  
  for(iter in 1:326){
    i <- allProposal[iter, 1]
    j <- allProposal[iter, 2]
    tempCipher <- swapGiven(givenCipher,i,j)
    allProposal[iter, 3] <- logLik(decodeText(cipheredText, tempCipher))
  }
  # result <- c()
  # result <- foreach(i=1:25) %:% foreach(j=(i+1):26) %dopar% {
  #   tempCipher <- swapGiven(givenCipher,i,j)
  #   logLik(decodeText(cipheredText, tempCipher))
  # }
  # length(result)
  # unlist(result)
  # allProposal[2:326,3] <- unlist(result) 
  # allProposal
  
  
  
  
  # all_scores <- as.numeric(allProposal[,3])
  prob <- exp(allProposal[-1,3])/sum(exp(allProposal[-1,3]))
  new_proposal <- sample(2:326, size = 1, prob, replace = TRUE)
  indicies_to_swap <- allProposal[new_proposal, 1:2]
  propCipher <- swapGiven(givenCipher, indicies_to_swap[1], indicies_to_swap[2])
  
  
  allProposalsProp[1,3]<- logLik(decodeText(cipheredText, propCipher))
  
  
  for(iter in 1:326){
    allProposalsProp[iter, 1] -> i 
    allProposalsProp[iter, 2] -> j
    tempCipher <- swapGiven(propCipher,i,j)
    allProposalsProp[iter, 3] <- logLik(decodeText(cipheredText, tempCipher))
  }
  # result <- c()
  # result <- foreach(i=1:25) %:% foreach(j=(i+1):26) %dopar% {
  #   tempCipher <- swapGiven(propCipher,i,j)
  #   logLik(decodeText(cipheredText, tempCipher))
  # }
  # length(result)
  # unlist(result)
  # allProposalsProp[2:326,3] <- unlist(result) 
  # allProposal
  # all_scores_new <- as.numeric(allProposalsProp[,3])
  q1 <- exp(allProposalsProp[1,3])/sum(exp(allProposalsProp[,3]))
  q2 <- exp(allProposal[new_proposal, 3])/ sum(exp(allProposal[,3]))
  # exp(allProposal[new_proposal, 3])
  return(list((propCipher), c(q1, q2)))
}










### FUNCTIONS DEPLOYING METROPOLIS ALGORITHM TO DERYPT A GIVRN TEXT IN GIVEN N ITERNATIONS ###

decryptMetrop <- function(cipheredText, n){
  currCipher <- generate_cipher()
  i <- 0
  currDecoded <- decodeText(cipheredText,
                                      cipher = currCipher)
  
  currLogLik <- logLik(currDecoded)
  max_score <- currLogLik
  bestCipher <- currCipher
  
  similar <- numeric(length = n)
  similar[1] <- currLogLik
  for (iter in 2:n) {
    
    propCipher <- new_sampling_way(currCipher)
    
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
      # print(glue::glue("Iteration {i}: {propDecoded}"))
      i <- i + 1
    }
    
    similar[iter] <- currLogLik
    
    if(currLogLik > max_score){
      max_score <- currLogLik
      bestCipher <- currCipher
    }
    if(iter %% (n/10) == 0){
      x <- iter*100/n
      print(paste(x, "% Completed"))
    }
    
  }
  bestDecoded <- decodeText(cipheredText,
                                   cipher = bestCipher)
  
  print(i/n)
  
  return(list(bestDecoded, similar))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
  
}



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
      # print(glue::glue("Iteration {i}: {propDecoded}"))
      i <- i + 1
    }
    
    similar[iter] <- currLogLik
    
    if(currLogLik > max_score){
      max_score <- currLogLik
      bestCipher <- currCipher
    }
    if(iter %% (n/10) == 0){
      x <- iter*100/n
      print(paste(x, "% Completed"))
    }
    
    
  }
  bestDecoded <- decodeText(cipheredText,
                                   cipher = bestCipher)
  
  print(i/n)
  
  return(list(bestDecoded, similar))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
}



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

    (q1 <- foo[[2]][1])
    (q2 <- foo[[2]][2])

    propDecoded <- decodeText(cipheredText,
                                         cipher = propCipher)
    currDecoded <- decodeText(cipheredText,
                                        cipher = currCipher)
    
    propLogLik <- logLik(propDecoded)
    currLogLik <- logLik(currDecoded)
    
  
    acceptProb <- min(1,(exp(propLogLik - currLogLik))*q1/q2)
    
    
    accept <- sample(c(TRUE, FALSE),
                     size=1,
                     prob = c(acceptProb,
                              1-acceptProb))
    
    if (accept) {
      currCipher <- propCipher
      currLogLik <- propLogLik
      # print(glue::glue("Iteration {i}: {propDecoded}"))
      i <- i + 1
    }
    
    similar[iter] <- currLogLik
    
    if(currLogLik > max_score){
      max_score <- currLogLik
      bestCipher <- currCipher
    }
    if(iter %% (n/10) == 0){
      x <- iter*100/n
      print(paste(x, "% Completed"))
    }
    
  }
  bestDecoded <- decodeText(cipheredText,
                                   cipher = bestCipher)
  
  # print(i/n)
  
  return(list(bestDecoded, similar, i/n))
  # save(war_and_peace_2_characters, file = "war_and_peace_2_characters.Rdata")
  
}




