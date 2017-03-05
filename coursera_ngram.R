library(quanteda)
library(readtext)
setwd("/home/dhachuel/Dropbox/Johns Hopkins Coursera/Capstone Project")  
src_file <- "final/en_US/en_US.news.txt"

crpus <- quanteda::corpus(x = readtext::readtext(file = src_file))

tokens <- toLower(
  quanteda::tokenize(crpus,
    removeNumbers = T,
    removePunct = T,
    removeSeparators = T,
    removeTwitter = T,
    ngrams = 4,
    concatenator = " ",
    simplify = T
  ) 
)

ngramSTR2Vector <- function(s){
  unlist(strsplit(s, " "))
}

m_ngrams <- as.data.frame(matrix(
  unlist(lapply(tokens, FUN=ngramSTR2Vector)), 
  ncol = 4, 
  byrow = TRUE
))
names(m_ngrams) <- c('input_1', 'input_2', 'input_3', 'input_4')


input_text <- "I want to get a lot of good athletes as far"
input_text <- tolower(input_text)
input_words <- tail(tokenizers::tokenize_words(input_text)[[1]],3)
sort(table(factor(m_ngrams[{
  m_ngrams$input_1 == input_words[1] &
  m_ngrams$input_2 == input_words[2] &
    m_ngrams$input_3 == input_words[3]  
},]$input_4)), decreasing = TRUE)[1:3]















con <- file("final/en_US/en_US.news.txt", "r")
raw <-readLines(con)
close(con)
##
## SETTINGS
##
NGRAM_MAX <- 4
NGRAM_MIN <- 2


##
## LOAN MODULES AND LIBRARIES
##
library(tokenizers)
library(doParallel)
library(foreach)

##
## SETUP CLUSTER
##
cl <- makeCluster(4)
doParallel::registerDoParallel(cl)

##
## TOKENIZE
##
raw.slice_len <- length(raw[1:200000])/4
# sentences <- foreach(i = 0:3) %dopar% unlist(tokenizers::tokenize_sentences({
#   raw[(raw.slice_len*i):(raw.slice_len*(i+1))]
# }))
sentences <- unlist(tokenizers::tokenize_sentences(raw[1:200000]))
rm(raw)
gc()
ngrams <- unlist(tokenizers::tokenize_ngrams(
  sentences, 
  n = NGRAM_MAX, 
  n_min = NGRAM_MIN,
  ngram_delim = " ", 
  simplify = TRUE  
))
rm(sentences)
gc()
##
## REMOVE STOP WORDS
##


##
## DETERMINE UNIQUE NGRAM COMBINATIONS
## 
ngramSTR2Vector <- function(s){
  unlist(strsplit(s, " "))
}
  
ngrams.v <- lapply(ngrams, FUN=ngramSTR2Vector)
rm(ngrams)
gc()



m_chain <- as.data.frame(matrix(unlist(ngrams.v), ncol = 4, byrow = TRUE))
names(m_chain) <- c('input', 'output')

unique_input_grams <- unique(m_chain[,1])
unique_output_grams <- unique(m_chain[,2])



##
## TRAIN MARKOV CHAIN
##
ngram_chain <- matrix(0, nrow = length(unique_input_grams), ncol = length(unique_output_grams))
rownames(ngram_chain) <- unique_input_grams
colnames(ngram_chain) <- unique_output_grams

for(ngram in ngrams.v){
  input_item = tolower(paste(ngram[1:length(ngram)-1], collapse=" "))
  output_item = tolower(paste(tail(ngram, n=1), collapse=" "))
  
  ngram_chain[input_item, output_item] <- ngram_chain[input_item, output_item] + 1
}

##
## MAKE ROW PERCENTS
##
ngram_chain.pct <- round(ngram_chain/rowSums(ngram_chain), 2)



##
## APPLY
##
input_text <- "Lorem IpsUm"

predict <- function(input_text){
  input_text <- tolower(input_text)
  input_words <- tokenizers::tokenize_words(input_text)[[1]]
  
  
  input_slices <- c()
  for(slice_size in seq(from = NGRAM_MIN-1, to = NGRAM_MAX-1, by = 1)){
    input_slices <- c(input_slices, paste(tail(input_words, n = slice_size), collapse=" "))
  }
  input_slices <- unique(input_slices)
  
  
  choices <- c()
  for(input_slice in input_slices){
    choices <- c(choices, sort(ngram_chain.pct[input_slice,], decreasing = TRUE)[1])
  }
  final_choice <- sort(choices, decreasing = TRUE)[1]
  output <- names(final_choice)
  
  return(output)
}

predict(input_text = "Cras iaculis") # Cras iaculis tortor
predict(input_text = "Cras iaculis tortor") # Cras iaculis tortor vitae
predict(input_text = "habitant morbi tristique senectus et") # habitant morbi tristique senectus et netus
















