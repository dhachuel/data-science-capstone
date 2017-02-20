# LOAD LIBRARIES
suppressMessages(require(BBmisc))
suppressAll({
  library(ggplot2)
  library(reshape2)
  library(tm)
  library(slam)
  library(wordcloud)
  library(tokenizers)
  library(stringi)
})

# SET WORKING DIRECTORY
setwd("/home/dhachuel/Dropbox/Johns Hopkins Coursera/Capstone Project")  

# BASIC PLOTS (histograms and similar to illustrate data features)
getWordStats <- function(input_text){
  cat("Tokenizing words...\n")
  input_text.words <- unlist(tokenizers::tokenize_words(input_text))
  input_text.words.table <- as.data.frame(table(input_text.words))
  
  cat("Sorting words by decresing frequencies...")
  input_text.words.table <- input_text.words.table[order(-input_text.words.table$Freq) , ]
  
  names(input_text.words.table)[1] <- "Words"
  ggplot(head(input_text.words.table, 10) , aes(x = Words, y = Freq)) + 
    geom_bar(colour="black", stat="identity") +
    ggtitle("Top 10 Words")
}

# BASIC SUMMARY (word counts, line counts and basic data tables)
getBasicTextSummary <- function(filepath){
  cat("Creating file connection...\n")
  con <- file(filepath, "rb")
  
  cat("Reading content from connection instance...\n")
  text <- readLines(con, encoding="UTF-8")
  
  cat(paste("Summary for file : ", summary(con)$description, "\n"))
  
  cat(paste(
    "Size : ",
    round(file.info(summary(con)$description)$size/1048576,2),
    " mb\n"
  ))
  close(con)
  
  general_stats <- stringi::stri_stats_general(text)
  cat(paste(
    "File contains ",
    general_stats['Lines'],
    " lines\n"
  ))
  cat(paste(
    "File contains ",
    general_stats['Chars'],
    " characters\n"
  ))
  
  cat("Tokenizing words in lines...\n")
  count_words <- stringi::stri_count_words(text)
  cat("Count words/line summary:\n")
  print(summary(count_words))
  cat("\n")
  print(qplot(count_words, main = paste(filepath," - Frequency of Words Per Line")))
  
  getWordStats(input_text = text)
}

getBasicTextSummary(filepath = "final/en_US/en_US.twitter.txt")
getBasicTextSummary(filepath = "final/en_US/en_US.blogs.txt")
getBasicTextSummary(filepath = "final/en_US/en_US.news.txt")






