generate_cipher <- function() sample(letters,
                                     replace = FALSE)

# Encode a text using a cipher
encode_text <- function(text, cipher) {
  chartr(
    x = text,
    old = paste(letters, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}
plaintext <- "the quick brown fox jumps over the lazy dog"

true_cipher <- generate_cipher()
ciphered_text <- encode_text(text = plaintext,
                             cipher = true_cipher)
source("function.R")

# Choose a value of n which will be the number of iterations
n <- 50000


# Choose the string which you would like to encrypt and then decrypt using metropolis algorithm


decoded_text_best <- encrypt_decrypt_using_metropolis(ciphered_text, n)
paste("The best decoded text reached in" , n , "iterations is: '")
decoded_text_best[[1]]
plot.ts(decoded_text_best[[2]])
decoded_text_best[[2]][1]
decoded_text_best[[2]][9999]

