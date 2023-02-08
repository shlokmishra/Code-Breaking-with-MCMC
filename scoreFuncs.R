#################### USING WAR AND PEACE TO CALCULATE BI-GRAM FREQUENCIES ####################
load("data.Rdata")
## data.Rdata in the repository already contains the probability table required to calculate the similarity score
## Hence the portion of code executing that part has been commented out 

#### UNCOMMENT THE FOLLOWING LINES TO IMPORT AND CLEAN WAR AND PEACE ###
# war_and_peace <- readr::read_file("https://www.gutenberg.org/cache/epub/2600/pg2600.txt")
# war_and_peace <-
#   war_and_peace |>
#   stringr::str_to_lower() |>
#   gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |>
#   stringi::stri_trans_general(id = "Latin-ASCII")

# Break a given string into two character substrings
break_into_two_chars <- function(text){
  starting_indices <- 1 : (nchar(text) - 1)
  ending_indices <- starting_indices + 1
  return(stringi::stri_sub(text,
                           from = starting_indices,
                           to = ending_indices))
}

#### UNCOMMENT THE FOLLOWING LINES TO GENERATE THE PROBABILITY TABLE ####
# war_and_peace_2_characters <- break_into_two_chars(war_and_peace)
# probability_table <-
  # table(war_and_peace_2_characters) / length(war_and_peace_2_characters)



# Obtain the probability of the two character string from probability table generated using War and Peace
get_prob_two_char <- function(two_char){
  prob_from_table <- probability_table[two_char]
  
  if (is.na(prob_from_table)) {
    return(0.5/ length(war_and_peace_2_characters))
  } else{
    return(prob_from_table)
  }
}


# Calculate log likelihood of a given piece of text
logLik <- function(text){
  text %>%
    break_into_two_chars() %>%
    purrr::map_dbl(get_prob_two_char) %>%
    log() %>%
    sum()
}


