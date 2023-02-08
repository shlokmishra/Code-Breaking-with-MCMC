giventext <- "a this is a sample peice of text and i am really glad that this code can easily crack it"


generate_cipher <- function() sample(letters,
                                     replace = FALSE)


encode_text <- function(text, cipher) {
  chartr(
    x = text,
    old = paste(letters, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}
swap <- function(x){
  # Select two distinct indices
  rand_indices <- sample(1:length(x), size = 2, replace=FALSE)
  element_1 <- x[rand_indices[1]]
  element_2 <- x[rand_indices[2]]
  
  x[rand_indices[1]] <- element_2
  x[rand_indices[2]] <- element_1
  
  return(x)
}

check <- function(cipher){
  if(!(current_cipher[1] == split_text[[1]][1] || current_cipher[9] == split_text[[1]][1])) {
    current_cipher <- generate_cipher()
    return(current_cipher)
  } 
}

decode_this <- encode_text(giventext, generate_cipher())
split_text <- strsplit(decode_this, "")
len <- length(split_text[[1]])
current_cipher <- generate_cipher()

if(split_text[[1]][1] != " "  && split_text[[1]][2] == " "){
  # which("w" == letters)
  # letters[26]
  num <- which(split_text[[1]][1] == letters)
   
  
  
}
paste(letters, collapse = "")

for(i in 2:(len-1)){
  if((split_text[[1]][i] != " "  && split_text[[1]][i+1] == " " ) && split_text[[1]][i-1] == " "){
    
  }
}
