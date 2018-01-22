rm(list=ls())

try(require(stringr) || install.packages("stringr"))
library(stringr)

try(require(dplyr) || install.packages("dplyr"))
library(dplyr)

require(tidytext) || install.packages("tidytext")
library(tidytext)

require(tm) || install.packages("tm")
library(tm)

try(require(wordcloud) || install.packages("wordcloud"))
library(wordcloud)

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

create_dtm <- function(text){

  corpus <- Corpus(VectorSource(text))
  DTM<-DocumentTermMatrix(corpus)
  return(DTM)  
}

build_wordcloud <- function(dtm){        # write within double quotes
  require(wordcloud)
  max.words1=200
  min.freq=2
  title1="Word Cloud"
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  # head(tsum)
  # tail(tsum)
  
  # windows()  # New plot window
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(3.5, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title1)     # title for the wordcloud display
  
} # func ends

