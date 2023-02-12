load("data/data50kIter.Rdata")
source("rFuncs/scoreFuncs.R")

paste("The original text was:")
plainText
paste("The best decoded text reached in" , n , "iterations is: '")
decodedBestReg[[1]]
decodedBestModified[[1]]

# Visualising the change in similarity score throughout the process
plot(decodedBestReg[[2]], ylim = range(c(decodedBestReg[[2]], logLik(plainText))),
     xlim = c(1, n), type = 'l')
lines(decodedBestModified[[2]], col = "red")
abline(h=logLik(plainText), col = "green")
