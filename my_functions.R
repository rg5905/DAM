rm(list=ls())

try(require(stringr) || install.packages("stringr"))
library(stringr)

try(require(dplyr) || install.packages("dplyr"))
library(dplyr)

require(tidytext) || install.packages("tidytext")
library(tidytext)

require(tm) || install.packages("tm")
library(tm)

try(require(tidyr) || install.packages("tidyr"))
library(tidyr)

require(tibble)



#Funtion to clean a text corpus
clean_text <- function(text){
  
  
  text  <-  gsub("<.*?>", " ", text)
  text <- tolower(text)   # Lowercase
  # Remove everything that is not a number or letter (may want to keep more stuff in your actual analyses). 
  text <- stringr::str_replace_all(text,"[^a-zA-Z\\s]", " ") # anything not alphabetical followed by a space, replace!
  # Shrink down to just one white space using '+' regex or for repeats >1
  text <- stringr::str_replace_all(text,"[\\s]+", " ") # collapse one or more spaces into one space.
  
  
  textdf = data_frame(text = text)
  
  data(stop_words)
  
  user_stop_words<-c("sentences","paragraphs","other")
  
  stop_words_all<-rbind(stop_words,data.frame(word=user_stop_words, lexicon="USER"))
  
  stop_words_all<-unique(stop_words_all)
  
  cleaned_text<- tm::removeWords(text, stop_words_all$word)
  
  return(cleaned_text)
}

