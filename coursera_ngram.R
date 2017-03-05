library(quanteda)
library(tokenizers)
setwd("C:/Users/n813863/Documents/CourseraCapstone")  
src_file <- "data/final/en_US/en_US.news.txt"
con <- file(src_file, "r")
raw <-readLines(con)
close(con)

##
## SETTINGS
##
NGRAM_MAX <- 5
NGRAM_MIN <- 5


##
## TOKENIZE
##
sentences <- unlist(tokenizers::tokenize_sentences(raw))
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
  
m_ngrams <- as.data.frame(matrix(
  unlist(lapply(ngrams, FUN=ngramSTR2Vector)), 
  ncol = NGRAM_MAX, 
  byrow = TRUE
))
names(m_ngrams) <- c('input_1', 'input_2', 'input_3', 'input_4', 'input_5')
rm(ngrams)
gc()




input_text <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
test_words <- c('eat', 'give', 'die', 'sleep')
# give WRONG -> die

input_text <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
test_words <- c('financial', 'marital', 'horticultural', 'spiritual')
# financial WRONG -> marital

input_text <- "I'd give anything to see arctic monkeys this"
test_words <- c('weekend', 'decade', 'month', 'morning')
# month WRONG -> weekend

input_text <- "Talking to your mom has the same effect as a hug and helps reduce your"
test_words <- c('happiness', 'hunger', 'stress', 'sleepness')
# hunger WRONG -> 

input_text <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
test_words <- c('look', 'minute', 'picture', 'walk')
# minute

input_text <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
test_words <- c('incident', 'case', 'account', 'matter')
# case WRONG -> matter

input_text <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
test_words <- c('arm', 'finger', 'toe', 'hand')
# hand

input_text <- "Every inch of you is perfect from the bottom to the"
test_words <- c('center', 'side', 'top', 'middle')
# top

input_text <- "I’m thankful my childhood was filled with imagination and bruises from playing"
test_words <- c('daily', 'outside', 'weekly', 'inside')
# outside

input_text <- "I like how the same people are in almost all of Adam Sandler's"
test_words <- c('novels', 'pictures', 'movies', 'stories')
# movies

input_text <- tolower(input_text)
input_words <- tail(tokenizers::tokenize_words(input_text)[[1]],4)
result.all <- sort(table(factor(m_ngrams[{
  {
    m_ngrams$input_1 == input_words[1] &
    m_ngrams$input_2 == input_words[2] &
    m_ngrams$input_3 == input_words[3] &
    m_ngrams$input_4 == input_words[4]
  } |
  {
    m_ngrams$input_2 == input_words[2] &
    m_ngrams$input_3 == input_words[3] &
    m_ngrams$input_4 == input_words[4]
  } |
  {
    m_ngrams$input_3 == input_words[3] &
    m_ngrams$input_4 == input_words[4]
  } |
  {
    m_ngrams$input_4 == input_words[4]
  }
    
},]$input_5)), decreasing = TRUE)


result.all <- sort(table(factor(m_ngrams[{
  {
      m_ngrams$input_4 == input_words[4]
  }
},]$input_5)), decreasing = TRUE)

# is.element('morning', names(result.all))
sort(result.all[test_words], decreasing=T)






##
## APPLY
##
input_text <- "Lorem IpsUm"

predict <- function(input_text){
}

predict(input_text = "Cras iaculis") # Cras iaculis tortor
predict(input_text = "Cras iaculis tortor") # Cras iaculis tortor vitae
predict(input_text = "habitant morbi tristique senectus et") # habitant morbi tristique senectus et netus
