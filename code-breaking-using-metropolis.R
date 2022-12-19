source("function.R")

# Choose a value of n which will be the number of iterations
n <- 100

# Choose the string which you would like to encrypt and then decrypt using metropolis algorithm
giventext <- "this is a sample peice of text and i am really glad that this code can easily crack it"


decoded_text_best <- encrypt_decrypt_using_metropolis(giventext)
paste("The best decoded text reached in" , n , "iterations is: '", decoded_text_best, "' ")
