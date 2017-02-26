raw <- "
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi faucibus lectus non sodales suscipit. Aliquam gravida ultrices nulla sed mattis. Pellentesque imperdiet nisl quis diam posuere placerat. Maecenas hendrerit dolor consectetur, mattis magna vitae, vehicula mauris. Vivamus ac commodo turpis. Donec dolor libero, faucibus ut eros nec, laoreet sagittis ex. Integer sagittis justo diam, ut dapibus enim congue quis. Donec finibus quis ipsum sit amet imperdiet. Curabitur interdum, quam in varius tempor, est leo scelerisque justo, eget semper urna purus eget libero. Mauris auctor nibh quis tortor scelerisque ultricies. Sed pharetra mauris vitae ultricies laoreet. Duis in orci in sapien vestibulum pulvinar sit amet nec ipsum.

Aliquam id lobortis neque, sit amet commodo enim. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Quisque euismod fringilla quam, vitae congue tellus auctor ac. Aliquam rutrum augue et odio placerat, in laoreet enim lobortis. Suspendisse ac massa ac nisi blandit condimentum. Nunc euismod, magna at gravida euismod, mauris ligula feugiat lacus, quis fermentum felis sapien sit amet ligula. Mauris non eros leo. Etiam in ligula nisl. Praesent sollicitudin, mi nec fringilla venenatis, ante lorem suscipit dolor, et pellentesque neque justo sed sapien. Aliquam lectus elit, lobortis sed orci at, placerat ornare ex. Ut nec nisl vehicula, aliquam odio et, finibus arcu.

Nulla sem mi, placerat eget commodo non, volutpat ut tellus. Nam suscipit dui in tortor convallis posuere. Fusce sagittis facilisis justo, nec aliquet lacus blandit at. In placerat aliquam nulla, rutrum congue enim. Donec eu felis vitae arcu commodo pulvinar. Sed gravida euismod erat. In hac habitasse platea dictumst. Curabitur sed accumsan urna, sed auctor eros. Nulla facilisi. Vestibulum pulvinar, purus quis dignissim dictum, orci mauris fringilla dolor, pellentesque consectetur velit elit vitae orci. Suspendisse tempus erat vel risus aliquam cursus. Quisque sagittis quam ac neque laoreet, eget fermentum sapien consequat.

Pellentesque nec orci vel arcu dapibus lobortis. Integer purus lorem, luctus in cursus quis, luctus pellentesque lacus. Vivamus fringilla viverra commodo. Nulla eu nisl eget nisi gravida blandit. In condimentum ligula in magna pretium, vel sodales ante ultricies. Nam cursus vitae sem vitae finibus. Integer et velit sed felis volutpat aliquam.

Integer nec risus vitae diam aliquet vestibulum a nec arcu. Sed a sem massa. Duis sapien nibh, rhoncus hendrerit suscipit facilisis, rutrum pharetra augue. Proin commodo ac turpis vitae fermentum. Etiam urna justo, interdum ut risus in, vehicula condimentum massa. Proin eu nunc nulla. Mauris non magna vel odio imperdiet interdum eget et neque. Curabitur nec nibh at sem porta porttitor at sit amet eros. Phasellus eget eros non diam convallis scelerisque vel ut ante. Proin dolor lorem, ullamcorper vel nibh eu, lobortis fermentum libero. Ut vel auctor erat. Donec ullamcorper posuere ex, dapibus rutrum diam feugiat sit amet. Sed ut nisl vel lorem eleifend tristique. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Ut pellentesque enim convallis ipsum lacinia, et feugiat turpis bibendum. Quisque sollicitudin convallis pretium.

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras iaculis tortor vitae vestibulum viverra. Donec pellentesque, nunc in tincidunt varius, lectus nunc tincidunt orci, eu mollis magna lacus non diam. Aliquam erat volutpat. Quisque gravida laoreet sapien ac sodales. Fusce imperdiet dignissim turpis non condimentum. Etiam pulvinar auctor libero, sed dignissim tellus malesuada at. In cursus tortor nunc, sed eleifend felis luctus vestibulum. Integer lobortis, neque a pretium pulvinar, ante enim sollicitudin tortor, ut mollis massa tortor maximus nisl. Vestibulum eleifend, tellus sed hendrerit eleifend, nunc quam ornare sapien, non hendrerit turpis leo et sapien. Pellentesque vel varius nisi. In interdum nisl ut scelerisque pharetra. In eget ultrices ligula. Curabitur eget rutrum ligula, faucibus volutpat nunc. Vestibulum eleifend lacus tellus, nec posuere nunc pulvinar quis. Donec in arcu fringilla, vehicula magna sed, semper nulla.
"

##
## SETTINGS
##
NGRAM_MAX <- 5
NGRAM_MIN <- 2


##
## LOAN MODULES AND LIBRARIES
##
library(tokenizers)

##
## TOKENIZE
##
sentences <- tokenizers::tokenize_sentences(raw)[[1]]
ngrams <- unlist(tokenizers::tokenize_ngrams(sentences, n = NGRAM_MAX, n_min = NGRAM_MIN))

##
## REMOVE STOP WORDS
##


##
## DETERMINE UNIQUE NGRAM COMBINATIONS
## 
ngramSTR2Vector <- function(s){
  result <- unlist(strsplit(s, " "))
  return(result)
}
  
ngrams.v <- lapply(ngrams, FUN=ngramSTR2Vector)

unique_input_grams <- c()
unique_output_grams <- c()

for(ngram in ngrams.v){
  input_item = tolower(paste(ngram[1:length(ngram)-1], collapse=" "))
  output_item = tolower(paste(tail(ngram, n=1), collapse=" "))

  unique_input_grams <- c(unique_input_grams, input_item)
  unique_output_grams <- c(unique_output_grams, output_item)
}

unique_input_grams <- unique(unique_input_grams)
unique_output_grams <- unique(unique_output_grams)



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
















