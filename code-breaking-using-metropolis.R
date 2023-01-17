source("function.R")

# Choose a value of n which will be the number of iterations
n <- 5000


# Choose the string which you would like to encrypt and then decrypt using metropolis algorithm
plaintext <- "the quick brown fox jumps over the lazy dog there are eight billion people in this world"


true_cipher <- generate_cipher()
ciphered_text <- encode_text(text = plaintext,
                             cipher = true_cipher)




decoded_text_best <- encrypt_decrypt_using_metropolis(ciphered_text, n)
paste("The best decoded text reached in" , n , "iterations is: '")
decoded_text_best[[1]]
plot.ts(decoded_text_best[[2]])
abline(h=get_log_lik_text(plaintext), col = "red")

