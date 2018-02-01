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

try(require(magrittr) || install.packages("magrittr"))
library("magrittr")


require(tibble)



#Funtion to clean a text corpus
clean_text <- function(text, user_stop_words=""){

  
  text  <-  gsub("<.*?>", " ", text)
  text <- tolower(text)   # Lowercase
  # Remove everything that is not a number or letter
  text <- stringr::str_replace_all(text,"[^a-zA-Z\\s]", " ") # anything not alphabetical followed by a space, replace!
  # Shrink down to just one white space using '+' regex or for repeats >1
  text <- stringr::str_replace_all(text,"[\\s]+", " ") # collapse one or more spaces into one space.
  
  
  
  data(stop_words)
  
  stop_words_all<-rbind(stop_words,data.frame(word=user_stop_words, lexicon="USER"))
  
  stop_words_all<-unique(stop_words_all)
  
  cleaned_text<- tm::removeWords(text, stop_words_all$word)
  
  textdf = data_frame(docid=1:length(cleaned_text), textcontent = cleaned_text)
  
  return(textdf)
}

create_dtm <- function(text,weighting_type="TF"){
  text_tidy <- text %>%
    unnest_tokens(word, textcontent) %>%
    count(docid, word, sort = TRUE) %>%
    ungroup()
  
  if(weighting_type=="TF"){
    DTM = cast_dtm(data=text_tidy,document=docid,term=word,value=n)
  } 
  if(weighting_type=="IDF"){
    DTM = cast_dtm(data=text_tidy,document=docid,term=word,value=n,weighting = tm::weightTfIdf)
  } 
  return(DTM)  
}



#-----------------------------------------------------------#
# A cleaned up or 'distilled' COG Plot            #
#-----------------------------------------------------------#

distill.cog = function(mat1, # input TCM ADJ MAT
                       title, # title for the graph
                       s,    # no. of central nodes
                       k1){  # max no. of connections  
  library(igraph)
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
} # distill.cog func ends



build_wordcloud <- function(dtm){        # write within double quotes
  require(wordcloud)
  max.words1=100
  min.freq=5
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
  
}  

build_barchart <- function(dtm){        # write within double quotes
  # visualize the commonly used words using ggplot2.
  library(ggplot2)
  library(dplyr)
  
  
  tidytext::tidy(dtm) %>%
    ungroup(document) %>% 
    count(term, sort = TRUE) %>%
    filter(n > 20) %>%   # n is wordcount colname. 
    mutate(term = reorder(term, n)) %>%  # mutate() reorders columns & renames too
    ggplot(aes(term, n)) +
    geom_bar(stat = "identity") +
#    xlab(NULL) +
    coord_flip()
  
}

display_dtm <- function(dtm){        # write within double quotes
  #Wordcloud
  build_wordcloud(dtm)
  
  #Co-occurence
  dtm1 = as.matrix(dtm)   # need it as a regular matrix for matrix ops like %*% to apply
  adj.mat = t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
  diag(adj.mat) = 0     # no self-references. So diag is 0.
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]])   # taking the top 50 rows and cols only
  
  #Call function
  distill.cog(adj.mat, 'Distilled COG - TF',  5,  5)
  
  #Bar Chart  
  print(build_barchart(dtm))
  

} # func ends




bigram.collocation <- function(text1){   # text1 from readLines() is input
  
  require(magrittr)
  require(tidytext)
  require(dplyr)
  require(tidyr)
  
  text1 = gsub('<.*?>', "", text1)   # drop html junk
  
  # create words df
  text_df <- data_frame(text1) %>% 
    unnest_tokens(word, text1) %>%
    anti_join(stop_words) %>% 
    count(word, sort = TRUE) #%>% 
  text_df
  
  # create bigrams df
  bigram_df <- data_frame(text1) %>% 
    unnest_tokens(bigrams, text1, token = "ngrams", n = 2) %>%
    count(bigrams, sort = TRUE) %>%
    ungroup() %>%
    
    # separate & filter bigrams for stopwords
    separate(bigrams, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(!(word1 %in% stop_words$word)) %>%
    dplyr::filter(!(word2 %in% stop_words$word)) #%>%
  
  bigram_df              
  
  # create a merged df
  new_df = bigram_df %>% mutate(k1 = 0) %>% mutate(k2 = 0) # %>%
  
  for (i1 in 1:nrow(bigram_df)){
    
    a0 = which(bigram_df$word1[i1] == text_df$word) 
    new_df$k1[i1] = text_df$n[a0]
    
    a1 = which(bigram_df$word2[i1] == text_df$word) 
    new_df$k2[i1] = text_df$n[a1]
    
  } # i1 loop ends
  
  new_df1 = new_df %>% filter(n > 1) %>% mutate(coll.ratio = (n*nrow(new_df))/(k1*k2)) %>%
    filter(coll.ratio >= 1) %>%
    unite(bigram_united, word1, word2) %>%
    arrange(desc(coll.ratio)) %>% 
    select(bigram_united, n, coll.ratio) 
  
  return(new_df1)
}   # func ends



concordance.r <- function(text1,  # corpus
                          word1,  # focal word for whcih context is sought
                          k){     # context window length in words on either side
  
  require(magrittr)
  require(tidytext)
  require(dplyr)
  require(tidyr)
  
  text1 = gsub('<.*?>', "", text1)   # drop html junk
  
  text_df <- data_frame(text1) %>% 
    unnest_tokens(word, text1) %>% 
    
    # build an index for word positions in the corpus
    mutate(index = 1) %>% mutate(wordnum = 1:sum(index)) %>% select(-index) #%>%
  
  text_df
  
  # locate context words for each instance of the focal word
  a0 = which(text_df$word == word1)
  a1 = matrix(0, nrow = length(a0), ncol = 3)
  colnames(a1) = c("start", "focal", "stop")
  for (i1 in 1:nrow(a1)){a1[i1, 1] = max(0, a0[i1]-k) 
  a1[i1, 2] = a0[i1]
  a1[i1, 3] = min(nrow(text_df), a0[i1]+k)  }
  head(a1)
  
  require(stringi)
  # creat a list to store the contexts or concordances of word1  
  list0 = vector("list", length = length(a0))
  for (i2 in 1:length(list0)){
    list0[[i2]] = stri_join(text_df$word[a1[i2,1]:a1[i2, 3]], collapse=" ") } # i2 ends
  list0[[2]]
  
  # read list into dataframe for easier display of output  
  list_df = data_frame("text")
  for (i2 in 1:length(a0)){list_df[i2,1] = list0[[i2]]}
  list_df
  
  return(list_df) } # func ends



