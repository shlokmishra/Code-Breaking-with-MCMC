load("data/data50kIter.Rdata")
source("rFuncs/metropFuncs.R")


paste("The best decoded text reached in" , n , "iterations is: '")
decodedBestReg[[1]]
decodedBestModifiedTest[[1]]

# Visualising the change in similarity score throughout the process
plot(decodedBestReg[[2]], ylim = range(c(decodedBestReg[[2]], logLik(plainText))),
     xlim = c(1, n), type = 'l')
lines(decodedBestModifiedTest[[2]], col = "green")
abline(h=logLik(plainText), col = "red")
