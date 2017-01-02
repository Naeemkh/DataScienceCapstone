
library(stringi)

# Load ngrams1
unigrams <- read.table("unigrams_total_2.txt", header = TRUE)
bigrams  <- read.table("bigrams_total_2.txt", header = TRUE)
trigrams <- read.table("trigrams_total_2.txt", header = TRUE)
tetragrams <- read.table("tetragrams_total_2.txt", header = TRUE)
pentagrams <- read.table("pentagrams_total_2.txt", header = TRUE)


# ------------------------- Sub functions -----------------------
# Cleaning the input data
cleanInput <- function(mystring){
  # remove numbers
  mystr <- gsub("[0-9]","",mystring)
  # remove puctuations
  mystr <- gsub("[[:punct:]]","",mystr)
  # remove initial and final space (if any)
  mystr <- gsub("^\\s+","",mystr)
  mystr <- gsub("\\s+$","",mystr)
  # convert it to lower char 
  mystr <- tolower(mystr)
  # convert any number of space to _
  mystr <- gsub("\\s+","_",mystr)
  return(mystr)
}

getBestUniGrams <- function(alpha,n=number,unigrams, obs.tail){
  
  alpha.prod <- prod(alpha)
  obs.removed.unigrams <- unigrams[!(unigrams$ngram %in% obs.tail),]
  unigrams.temp.prob  <- alpha.prod * ((obs.removed.unigrams$freq) /sum(obs.removed.unigrams$freq))
  unigrams.prob <- data.frame(ngram =obs.removed.unigrams$ngram, prob = unigrams.temp.prob )
  unigrams.prob.pred <- unigrams.prob[1:n,] 
  return(unigrams.prob.pred)
  
}

countString <- function(cleanedString){
  word_list <- stri_split_fixed(cleanedString,"_")
  length(word_list[[1]])
}

getNGrams <- function(mystring,n,alpha,obs.tail,ngrams, gamma){
 
  regex.pattern   <- sprintf("%s%s%s","^",mystring,"_")
  
  
  if (is.null(obs.tail)){
    index <- grep(regex.pattern,ngrams$ngram)
  }else{
    observed.pattern <- sprintf("%s%s%s",mystring,"_",obs.tail)
    index <- grep(regex.pattern,ngrams[!(ngrams$ngram %in% observed.pattern),]$ngram)
  }
  
  if (length(index) == 0) {
    obs.temp.prob <- 0
    obs.prob.pred <- data.frame(ngram=character(length = 0L),prob=double(length = 0L ))
    
    
  } else { 
    count <- sum(ngrams[index,]$freq)
    obs.temp.prob  <- prod(alpha) *((ngrams[index,]$freq - gamma) /count) 
    obs.temp.ngram  <- gsub(".*_","",ngrams[index,]$ngram)
    obs.prob <- data.frame(ngram=obs.temp.ngram,prob=obs.temp.prob)
    
    # Pick the first n words and save it as observed prediction
    if (nrow(obs.prob) > n){
      obs.prob.pred <- obs.prob[1:n,]
      
    } else {
      obs.prob.pred <- obs.prob  
    }
  }
  
  # Now we need to compute the alpha for the first value.
  current.alpha <- 1 - sum(obs.temp.prob) 
  
  alpha <- c(alpha,current.alpha)
  
  # find the observed tails.
  # We have two options here:
  #         1: Don't carry the observed words to other level.
  #         2: Carry the observed words to the other level.
  
  current.obs.tail   <-  gsub(".*_","", ngrams[index,]$ngram)
  all.obs.tail <- c(current.obs.tail, obs.tail)
  
  # string for lower level function
  numString <-  countString(mystring) 
  str_tmp <- stri_split_fixed(mystring,"_")[[1]]
  lastN_1 <- tail(str_tmp, n=numString-1)
  stringL  <- paste(lastN_1, collapse = '_')
  
  returnList <- list("alpha"=alpha,
                     "obs.tail" = all.obs.tail,
                     "obs.prob.pred" = obs.prob.pred,
                     "stringL" = stringL)
  
  return(returnList)
} 

getNextWordProb <- function(mystring,n,unigrams,bigrams,trigrams,tetagrams,pentagrams){
  
  # alpha initial value
  alpha <- 1
  
  # clean the string
  theInputString <- cleanInput(mystring) 
  
  # number of strings
  numString <-  countString(theInputString )     
  
  # if number of string more than 4, pick last 4 string
  if (numString > 4){
  str_tmp <- stri_split_fixed(theInputString,"_")[[1]]
  lastTwo <- tail(str_tmp, n=4)
  ngramsStr  <- gsub("\\s+","",paste(lastTwo[1],"_",lastTwo[2],"_",lastTwo[3],"_",lastTwo[4]))
  numString <- 4
  }else{
    ngramsStr <- theInputString   
  }
 
  if (numString == 4){
     
    pentaGrams <- getNGrams(ngramsStr,n,alpha=1,obs.tail=NULL,pentagrams, gamma=0.5)
    tetraGrams <- getNGrams(pentaGrams$stringL ,n,pentaGrams$alpha,obs.tail=pentaGrams$obs.tail, tetragrams, gamma=0.5)
    triGrams   <- getNGrams(tetraGrams$stringL ,n,tetraGrams$alpha,obs.tail=tetraGrams$obs.tail, trigrams, gamma=0.5)
    biGrams    <- getNGrams(triGrams$stringL ,n, triGrams$alpha,obs.tail=triGrams$obs.tail,bigrams, gamma=0.5)
    
    ngrams.data <- rbind(pentaGrams$obs.prob.pred,
                         tetraGrams$obs.prob.pred,
                         triGrams$obs.prob.pred,
                         biGrams$obs.prob.pred)
    
    ngrams.data <- ngrams.data[order(-ngrams.data$prob),]
                      
    if (nrow(ngrams.data) < n){
      bestUniGrams <- getBestUniGrams(alpha,n,unigrams, obs.tail=biGrams$obs.tail)
      ngrams.data <- rbind(bestUniGrams,ngrams.data)
      ngrams.data <- ngrams.data[order(-ngrams.data$prob),]
      ngrams.data <- ngrams.data[!duplicated(ngrams.data$ngram),]
      ngrams.data <- ngrams.data[1:n,]
      return(ngrams.data)
    }else{
      ngrams.data <- ngrams.data[!duplicated(ngrams.data$ngram),]
      ngrams.data <- ngrams.data[1:n,]
      return(ngrams.data)
    }
    
  }else if(numString == 3){
    tetraGrams <- getNGrams(ngramsStr,n,alpha=1,obs.tail=NULL,tetragrams, gamma=0.5)
    triGrams   <- getNGrams(tetraGrams$stringL ,n,tetraGrams$alpha,obs.tail=tetraGrams$obs.tail, trigrams, gamma=0.5)
    biGrams    <- getNGrams(triGrams$stringL ,n, triGrams$alpha,obs.tail=triGrams$obs.tail,bigrams, gamma=0.5)
    
    ngrams.data <- rbind(tetraGrams$obs.prob.pred,
                         triGrams$obs.prob.pred,
                         biGrams$obs.prob.pred)
    
    ngrams.data <- ngrams.data[order(-ngrams.data$prob),]
    
    if (nrow(ngrams.data) < n){
      bestUniGrams <- getBestUniGrams(alpha,n,unigrams, obs.tail=biGrams$obs.tail)
      ngrams.data <- rbind(bestUniGrams,ngrams.data)
      ngrams.data <- ngrams.data[order(-ngrams.data$prob),]
      ngrams.data <- ngrams.data[!duplicated(ngrams.data$ngram),]
      ngrams.data <- ngrams.data[1:n,]
      return(ngrams.data)
    }else{
      ngrams.data <- ngrams.data[!duplicated(ngrams.data$ngram),]
      ngrams.data <- ngrams.data[1:n,]
      return(ngrams.data)
    }
    
  }else if(numString == 2){
    
    triGrams   <- getNGrams(ngramsStr,n,alpha=1,obs.tail=NULL,trigrams, gamma=0.5)
    biGrams    <- getNGrams(triGrams$stringL ,n, triGrams$alpha,obs.tail=triGrams$obs.tail,bigrams, gamma=0.5)
    
    ngrams.data <- rbind(triGrams$obs.prob.pred,
                         biGrams$obs.prob.pred)
    
    ngrams.data <- ngrams.data[order(-ngrams.data$prob),]
    
    if (nrow(ngrams.data) < n){
      bestUniGrams <- getBestUniGrams(alpha,n,unigrams, obs.tail=biGrams$obs.tail)
      ngrams.data <- rbind(bestUniGrams,ngrams.data)
      ngrams.data <- ngrams.data[order(-ngrams.data$prob),]
      ngrams.data <- ngrams.data[!duplicated(ngrams.data$ngram),]
      ngrams.data <- ngrams.data[1:n,]
      return(ngrams.data)
    }else{
      ngrams.data <- ngrams.data[!duplicated(ngrams.data$ngram),]
      ngrams.data <- ngrams.data[1:n,]
      return(ngrams.data)
    }
    
  }else if(numString == 1){
      biGrams    <- getNGrams(ngramsStr,n,alpha=1,obs.tail=NULL,bigrams, gamma=0.5)
    
    ngrams.data <- rbind(biGrams$obs.prob.pred)
    
    ngrams.data <- ngrams.data[order(-ngrams.data$prob),]
    
    if (nrow(ngrams.data) < n){
      bestUniGrams <- getBestUniGrams(alpha,n,unigrams, obs.tail=NULL)
      ngrams.data <- rbind(bestUniGrams,ngrams.data)
      ngrams.data <- ngrams.data <- ngrams.data[order(-ngrams.data$prob),]
      ngrams.data[!duplicated(ngrams.data$ngram),]
      ngrams.data <- ngrams.data[1:n,]
      return(ngrams.data)
    }else{
      ngrams.data <- ngrams.data[!duplicated(ngrams.data$ngram),]
      ngrams.data <- ngrams.data[1:n,]
      return(ngrams.data)
    }
  }
  
}
