# Code to automatically generate keywords for data catalogue fuzzy matching

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  udpipe,
  dplyr,
  readr,
  readxl,
  stringr
)


# Reads in list of titles and exclusion words 
df1 <- read_csv("Data_Names.csv")


# Reads in exclusion list
exclusion_words <- read_excel("exclusion_words_and_ngrams.xlsx", 
                              sheet = "exclusion_words")

# Reads in list of ngrams .csv
ngrams <- read_excel("exclusion_words_and_ngrams.xlsx", sheet = "ngrams")


# Downloads language model
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)


# Splits titles by word and assigns word type
system.time(
  word_types <- udpipe_annotate(ud_model, x = df1$Title)
)
word_types <- as.data.frame(word_types)


# Sets all words to lower case
word_types <- word_types %>% mutate(token = tolower(token))


# Assigns selection priority based on word type
word_types <- word_types %>% 
  mutate(
    order = case_when(
      upos %in% c("PROPN", "NOUN", "VERB", "ADJ", "X", "AUX") ~ 1,
      TRUE ~ 3
    )
  )


# Reduces priority for words in the exclusion list
for (i in 1:nrow(exclusion_words)) {
  word_types$order <- ifelse(
    grepl(exclusion_words$word[i], word_types$token, fixed = "TRUE") == "TRUE",
    2,
    word_types$order
    )
}


# Create column with concatenation of word in row and row below 
for (k in 1:(nrow(word_types) - 1)) {
  word_types$phrase[k] <- paste(word_types$token[k], 
                                word_types$token[k + 1], 
                                sep = " ")
}


# Joins concatenated column to list of ngrams
word_types <- left_join(word_types, ngrams, by = "phrase")


# Loops over list to add in ngrams
for (j in 1:(nrow(word_types) - 1)) {
  
  # If flag is 1 then replace the word with the ngram 
  if(is.na(word_types$flag[j])) {word_types$flag[j] = FALSE} else { 
    if(word_types$flag[j] == 1) {
      
      word_types$token[j] <- word_types$phrase[j]
      word_types$order[j] <- 1
      
      # Flags row below for deletion
      word_types$flag[j + 1] <- 2
      
    }
  }
}


# Deletes redundant rows
word_types <- word_types %>% filter(flag != 2)


# Reduces priority for words one letter long
word_types$length <- str_count(word_types$token)
word_types$order <- ifelse(word_types$length == 1 , 4, word_types$order)


# Defines output dataframe
output <- matrix(ncol = 2, nrow = nrow(df1))
output <- as.data.frame(output) 


# Loops for every title
for (x in 1:nrow(word_types)) {
  
  # Selects rows for corresponding title
  df2 <- word_types %>% filter(doc_id == paste("doc", x, sep = ""))
  
  # Skip those filtered out in the line above (they will have a df with 0 rows)
  if (dim(df2)[1] > 0) {
    
    # Sorts based on priority
    df2 <- arrange(df2, order) 
    
    # Selects two highest priority words
    keywords <- head(df2[,c(6, 15)], 2)
    keywords <- as.data.frame(keywords)
    
    # Checks if only one word is presence 
    if (nrow(keywords) == 1) {
      
      # Assigns word to output dataframe
      output[x, 1] <- head(keywords, 1)
      output[x, 2] <- ""
      
    } else {
      
      # Checks if both keywords are the same
      if (grepl(head(keywords$token, 1), tail(keywords$token, 1), fixed = "TRUE")) {
        
        # Selects three highest priority words
        keywords <- head(df2[,c(6, 15)], 3)
        keywords <- as.data.frame(keywords)
        
      }
      
      # Assigns words to output dataframe
      output[x, 1] <- head(keywords$token, 1)
      output[x, 2] <- tail(keywords$token, 1)
      
    }
  }
}

output$title <- df1$Title


# Copy to clipboard
write.excel <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  write.table(x,
              "clipboard-16384",
              sep = "\t",
              row.names = row.names,
              col.names = col.names,...)
}

write.excel(output)
