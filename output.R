load("data/data-1k-Iter.Rdata")
source("rFuncs/scoreFuncs.R")

paste("The original text was:")
plainText
paste("The best decoded text reached in" , n , "iterations is: '")
decodedBestReg[[1]]
decodedBestBarker[[1]]
decodedBestLit[[1]]

# Visualising the change in similarity score throughout the process
plot(decodedBestReg[[2]], ylim = range(c(decodedBestReg[[2]], logLik(plainText))),
     xlim = c(1, n), type = 'l', xlab = "Log-Likelihoods", ylab = "Number of Iterations", main = "Comparing different MH algorithms")
lines(decodedBestBarker[[2]], col = "red")
lines(decodedBestLit[[2]], col = "blue")

abline(h=logLik(plainText), col = "green")
legend("bottomright", legend = c("Reg", "Barker", "Lit"),lty = c(1,1,1) , col = c("black", "red", "blue")) 

