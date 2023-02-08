#################### CIPHER FUNCTIONS ####################

# Generate a random cipher
generateCipher <- function() sample(letters, replace = FALSE)

# Encode a given piece of text using a cipher
encodeText <- function(text, cipher){
  chartr(
    x = text,
    old = paste(letters, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}

# Decoding ciphered text using a cipher
decodeText <- function(ciphered_text, cipher) {
  chartr(
    x = ciphered_text,
    old = paste(cipher, collapse = ""),
    new = paste(letters, collapse = "")
  )
}


#################### SWAP FUNCTIONS ####################

###SWAP ANY TWO RANDOM CIPHERS####
swapRand <- function(x){
  rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
  element_1 <- x[rand_indices[1]]
  element_2 <- x[rand_indices[2]]
  
  x[rand_indices[1]] <- element_2
  x[rand_indices[2]] <- element_1
  
  return(x)
}


###SWAP TWO GIVEN CIPHERS####
swapIndicies <- function(given_cipher, i, j){
  temp_cipher <- given_cipher
  element_1 <- temp_cipher[i]
  element_2 <- temp_cipher[j]
  
  temp_cipher[i] <- element_2
  temp_cipher[j] <- element_1
  return(temp_cipher)
}

