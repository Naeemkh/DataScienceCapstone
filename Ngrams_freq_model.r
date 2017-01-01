## Word-Frequency Model
### Based on KBO alghorithm
### Naeem Khoshnevis
### December 10, 2016.

# This script generats ngrams model. I load three data sets, and
# remove punctuations, numbers, URL, twitter. Convert all chars to 
# a lower case char. Then generate ngrams and count the numbers of 
# occurance in the corpus. Due to memory issue, I load data, by chunk
# then I merge the ngrams and add up the frequencies.




rm(list=ls())
#### Computing ngrams
library(quanteda)
library(dplyr)
library(data.table)

# Function to generat ngrams matrix
ngramsFreq <- function(data, ngram,  ignores=NULL, saveData=FALSE, filename) {
  
  file.dfm <- dfm( data, ngrams=ngram,
                   ignoredFeatures=ignores,
                   removePunct = TRUE,
                   removeNumbers = TRUE,
                   removeURL = TRUE,
                   removeTwitter = TRUE,
                   toLower = TRUE,
                   language = "english")
  
  frq <- apply(as.data.frame(file.dfm),2, sum)
  ngrams_data <- data.table(ngram=names(frq), freq=frq)
  #sort data
  ngrams_data <- ngrams_data[order(-ngrams_data$freq),]
  
  # save data
  if (saveData){
    write.table(ngrams_data,file = filename, sep = " ",
                quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
  }
  
  return(ngrams_data)
  
}


enUsBlog <- readLines('../finalData/en_US/en_US.blogs.txt',skipNul = TRUE)
enUsTwit <- readLines('../finalData/en_US/en_US.twitter.txt',skipNul = TRUE)
enUsNews <- readLines('../finalData/en_US/en_US.news.txt',skipNul = TRUE)


### Sample data 
sampleData <- function(data1,data2,data3,samplePercent){
  sampleData1 <- sample(data1, floor(length(data1)*samplePercent))
  sampleData2 <- sample(data2, floor(length(data2)*samplePercent))
  sampleData3 <- sample(data3, floor(length(data3)*samplePercent))
  sampleData  <- c(sampleData1,sampleData2,sampleData3)
}


### generate ngrams, save them, and clear the memory

profdictionary <- readLines('../profanitywords.txt')
set.seed(1234)
sampled_data_10 <- sampleData(enUsNews, enUsTwit, enUsBlog, 0.9)

#nrow.data  <- length(sampled_data_10)

# Merge function
mergeNgrams <- function(allngrams){
  allngrams[is.na(allngrams$freq.x),]$freq.x <- 0
  allngrams[is.na(allngrams$freq.y),]$freq.y <- 0
  freq.temp <- allngrams$freq.x + allngrams$freq.y
  allngrams$freq.x <- NULL
  allngrams$freq.y <- NULL
  allngrams$freq <- freq.temp
  return(allngrams)
}

length(sampled_data_10)

index.2 <- 0
last.step <- 1
for (i in 1:last.step){
  print(paste("Step: ",i," out of :", last.step))
  index.1  <- index.2+1
  index.2  <- i*1000
  subdata  <- sampled_data_10[index.1:index.2]
  unigrams <- ngramsFreq(subdata,ngram = 1, saveData = FALSE, ignores = profdictionary, filename="unigram.txt")
  bigrams  <- ngramsFreq(subdata,ngram = 2, saveData = FALSE, ignores = profdictionary, filename="bigram.txt")
  trigrams <- ngramsFreq(subdata,ngram = 3, saveData = FALSE, ignores = profdictionary, filename="trigram.txt")
  tetragrams <- ngramsFreq(subdata,ngram = 4, saveData = FALSE, ignores = profdictionary, filename="tetragram.txt")
  pentagrams <- ngramsFreq(subdata,ngram = 5, saveData = FALSE, ignores = profdictionary, filename="pentagram.txt")
  
  if (i==1){
    allunigrams <- unigrams
    allbigrams  <- bigrams
    alltrigrams <- trigrams
    alltetragrams <- tetragrams
    allpentagrams <- pentagrams
  }else{
    allunigrams.temp  <- merge(unigrams,allunigrams, by="ngram", all=TRUE )
    allbigrams.temp   <- merge(bigrams,allbigrams, by="ngram", all=TRUE )
    alltrigrams.temp  <- merge(trigrams,alltrigrams, by="ngram", all=TRUE )
    alltetragrams.temp  <- merge(tetragrams,alltetragrams, by="ngram", all=TRUE )
    allpentagrams.temp  <- merge(pentagrams,allpentagrams, by="ngram", all=TRUE )
    
    allunigrams       <- mergeNgrams (allunigrams.temp)
    allbigrams        <- mergeNgrams (allbigrams.temp)
    alltrigrams       <- mergeNgrams (alltrigrams.temp)
    alltetragrams     <- mergeNgrams (alltetragrams.temp)
    allpentagrams     <- mergeNgrams (allpentagrams.temp)
    
  }
}


allunigrams <- allunigrams[order(-allunigrams$freq),]
allbigrams <- allbigrams[order(-allbigrams$freq),]
alltrigrams <- alltrigrams[order(-alltrigrams$freq),]
alltetragrams <- alltetragrams[order(-alltetragrams$freq),]
allpentagrams <- allpentagrams[order(-allpentagrams$freq),]

allunigrams
allbigrams
alltrigrams

write.table(allunigrams,file = "Eunigrams.txt", sep = " ",
            quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
write.table(allbigrams,file = "Ebigrams.txt", sep = " ",
            quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
write.table(alltrigrams,file = "Etrigrams.txt", sep = " ",
            quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
write.table(alltetragrams,file = "Etetragrams.txt", sep = " ",
            quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
write.table(allpentagrams,file = "Epentagrams.txt", sep = " ",
            quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")




# > length(sampled_data_10) (Engrams)
# [1] 3842709 (90 % of data)
# > 500000/3842709*0.9
# [1] 0.1171049 of total data 
