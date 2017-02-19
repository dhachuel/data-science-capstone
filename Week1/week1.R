WD <- "/home/dhachuel/Dropbox/Johns Hopkins Coursera/Capstone Project"
setwd(WD)  
con <- file("final/en_US/en_US.twitter.txt", "r")

linn <-readLines(con)
love_cnt <- 0
hate_cnt <- 0

for (i in 1:length(linn)){
  if(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", linn[i])){
    print(linn[i])
  }
}
print(love_cnt/hate_cnt)

close(con)


