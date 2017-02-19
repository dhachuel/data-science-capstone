# LOAD LIBRARIES
library(ggplot2)
library(reshape2)
library(tm)
library(slam)
library(wordcloud)
library(tokenizers)
library(doParallel)
doParallel::registerDoParallel(cores=4)

# SET WORKING DIRECTORY
setwd("/home/dhachuel/Dropbox/Johns Hopkins Coursera/Capstone Project")  

# INITIALIZE FILE CONNECTIONS
twitter.con <- file("final/en_US/en_US.twitter.txt", "r")
blogs.con <- file("final/en_US/en_US.blogs.txt", "r")
news.con <- file("final/en_US/en_US.news.txt", "r")

# READ DATA
twitter.text <- readLines(twitter.con, encoding="UTF-8")
close(twitter.con)

# BASIC SUMMARIES (word counts, line counts and basic data tables)


# BASIC PLOTS (histograms and similar to illustrate data features)