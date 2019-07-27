## 
## Library of functions used for word prediction cap stone project
##

getFileInfo <- function(directory) {
  df <- data.frame(name = c(), size = c())

    for (locale in locales) {
    for (context in contexts) {
      fileName <- paste(locale, context, fileExt, sep = fileNameSep)
      fullQualifiedFileName <- paste(directory, locale, fileName, sep = filePathSep)
      if (file.exists(fullQualifiedFileName) == TRUE) {
        fInfo <- file.info(fullQualifiedFileName)
        fileSizeInMb <- paste(round(fInfo$size / 1024 / 1024, 2), "MB")
        df <- rbind(df, data.frame(name = fileName, size = fileSizeInMb))
      } else {
        stop("File not found!") 
      }
    }
  }
  df
}


makeReducedData <- function(fileName, factor) {
  connection <- file(fileName, "rb")
  contents <- readLines(connection, encoding = "UTF-8", skipNul = TRUE)
  newContents <- sample(contents, length(contents) * factor)
  on.exit(close(connection))
  newContents
}



getLastNwords <- function(txt, n, seperator = " ") {
  #print("getLastNwords begins here: ")
  startTime <- proc.time()
  
  txtElems <- strsplit(txt, seperator)[[1]]
  if (length(txtElems) < n) {
    txt
    ##stop("Text length invalid.")
  } else {
    lowerBound <- (length(txtElems) - n + 1)
    txtElems <- txtElems[lowerBound:length(txtElems)]
  }
  lastWords <- paste(txtElems, collapse = " ")

    stopTime <- proc.time()
  #print("Elapsed time of getLastNwords: ")
  stopTime - startTime

    txt <- tolower(lastWords)
}


## Get last word out of a vector of strings
getLastWords <- function(txts) {
  print("getLastWords begins here: ")
  startTime <- proc.time()

    lastWords <- c()
  if (length(txts) != 0) {
    for(i in c(1:length(txts)))
      lastWords[i] <- getLastWord(txts[i])
  }
    stopTime <- proc.time()
    print("Elapsed time of getNgram gram")
    stopTime - startTime

    lastWords
}


## Get last word out of a string
getLastWord <- function (txt, seperator = " ") {
  txtElem <- strsplit(txt, seperator)[[1]]
  txtElem[length(txtElem)]
}


##
## Match search text with entries in N Gram data.frame
##
filterNgrams <- function(nGramDf, searchTxt) {
  # Will perl = TRUE incure performance issue ??? Or is it relevant ???
  nGramDf[grep(paste("^", searchTxt, " ", sep = ""), nGramDf$Term, perl = TRUE), ][, c("Term")]
}

##
## Given a text string as input, predict the 3 following possible words
##
getNextWordsSuggestion <- function(inputTxt) {
  suggestedWords <- c()
  nGramDfNames <- c("fiveGramDf", "fourGramDf", "triGramDf", "biGramDf", "oneGramDf") # 4 3 2 1 0
  for (i in 1:length(nGramDfNames)) {
    lowerBound <- 5 - i
    if (getLengthOfWords(inputTxt) < lowerBound) {
      next
    } else {
      if (nGramDfNames[i] == nGramDfNames[5]) {
        suggestedWords <- c(suggestedWords, get(nGramDfNames[i])[1:3, "Term"])
      } else {
        lastNwords <- getLastNwords(inputTxt, lowerBound)
        suggestedWords<- c(suggestedWords, 
                           getLastWords(filterNgrams(get(nGramDfNames[i]), lastNwords)))
      }
    }
  }
  suggestedWords <- subset(suggestedWords, !(suggestedWords %in% stopwords()))
  suggestedWords <- unique(suggestedWords)
  suggestedWords[1:3]
}

##
## Read Data from blog, news and twitter files and sample it based on sampling factor
##
mySampleData <- function(WorkingDirectory, sf,fileName){

  print("mySampleData begins here: ")
  startTime <- proc.time()
  startTime
  
  setwd("D:/Work/mystuff/Education/DataScience/DataScienceCapstone/Data")
  setwd(WorkingDirectory)

  SizeB  <- file.info("./final/en_US/en_US.blogs.txt")$size / 1024.0 / 1024.0
  SizeN  <- file.info("./final/en_US/en_US.news.txt")$size / 1024.0 / 1024.0
  SizeT  <- file.info("./final/en_US/en_US.twitter.txt")$size / 1024.0 / 1024.0
  
  
  bd <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul=TRUE)
  nd <- readLines("./final/en_US/en_US.news.txt",  encoding = "UTF-8", skipNul=TRUE)
  td <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul=TRUE)

  ## Calculating the word count
  WordsB <- sum(stri_count_words(bd))
  WordsN <- sum(stri_count_words(nd))
  WordsT <- sum(stri_count_words(td))
  
  
  library(stringi)
  ##sf <- 0.08
  ##sf <- 0.08
  ## Checking the length of the files 
  
  
  LengthB <- length(bd)
  LengthN <- length(nd)
  LengthT <- length(td)

  WordsperlinesB <- WordsB/LengthB
  WordsperlinesN <- WordsN/LengthN
  WordsperlinesT <- WordsT/LengthT
  
  
  SampleB <- sample(bd, size=LengthB*sf, replace=FALSE)
  SampleN <- sample(nd, size=LengthN*sf, replace=FALSE)
  SampleT <- sample(td, size=LengthT*sf, replace=FALSE)
  
  rm(td)
  rm(bd)
  rm(nd)

  ## Combine samples and permutate
  mySample <- c(SampleB, SampleN, SampleT)
  mySample <- sample(mySample, size=length(mySample), replace=FALSE)

  ## Save sample data
  writeLines(mySample, fileName )
  
  rm(SampleB)
  rm(SampleN)
  rm(SampleT)
  gc()
  
  SizeSD  <- file.info("mySample.txt")$size / 1024.0 / 1024.0
  LengthSD <- length(mySample)
  WordsSD <- sum(stri_count_words(mySample))
  
  WordsperlinesSD <- WordsSD/LengthSD
  
  myfileSummary <- data.frame(
    fileName = c("Blogs","News","Twitter", "Sample"),
    
    fileSize = c(round(SizeB, digits = 2), 
                 round(SizeN,digits = 2), 
                 round(SizeT, digits = 2),
                 round(SizeSD, digits = 2)
    ),
    
    lineCount = c(LengthB, LengthN, LengthN, LengthSD),
    
    wordCount = c(WordsB, WordsN, WordsT, WordsSD),
    wordperlines = c(WordsperlinesB, WordsperlinesN, WordsperlinesT, WordsperlinesSD )
    
  )
  
  colnames(myfileSummary) <- c("File_Name", "File_Size_MB", "Line_Count", "Word_Count", "Words_per_line")
  
  saveRDS(myfileSummary, file = "FileDataSummary.rds")
  
  myfileSummary
  
  stopTime <- proc.time()
  print("Elapsed time of mySampleData: ")
  stopTime - startTime
  
  
  return(mySample)
    
}

## Sampling data
##  sf_td := 0.8
##  sf_vd := 0.1
##  sf_tstd := 0.1


myTrainData <- function(mySample,sf_td, sf_vd, sf_tstd){

  print("Sampling data  begins here: ")
  startTime <- proc.time()

    
  trainP <- sf_td
  validateP <- sf_vd
  testP  <- sf_tstd
  
  testSplit <- testP 

  library(caret)
  
  ##gc(): this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being used by R.
  gc()
  
  
  ## Split the non-test data into training and validation data sets
  testSplit <- testP 
  trainSplit <- trainP /(trainP + validateP )
  
  TestDP <- createDataPartition(seq_len(NROW(mySample)), p=testSplit, list=FALSE)
  
  TestData <- mySample[TestDP]
  
  NonTestData <- mySample[-TestDP]
  rm(TestDP)
  
  
  ## Split the non-test data into training and validation data sets
  TrainDP <- createDataPartition(seq_len(NROW(NonTestData)), p=trainSplit,     list=FALSE)
  TrainData <- NonTestData[TrainDP]
  ValidateData <- NonTestData[-TrainDP]

  saveRDS(TrainData,   "TrainData.rds")
  saveRDS(TestData  ,   "TestData.rds")
  saveRDS(ValidateData, "ValidateData.rds")
  
  rm(TrainDP)
  rm(TestData)
  rm(ValidateData)
  rm(NonTestData)

  stopTime <- proc.time()
  print("Elapsed time of Data Sampling")
  stopTime - startTime
  
  return(TrainData)
}

## Clean Data 
## Build clean Corpus as follow:
## 1. Convert test to plain tex document
## 2. Conver text to lowercase
## 3. replacing contractions with their full forms
## 4. Remove profanities, numbers and punctuation.
## 5. Remove URLs
## 6. Remove numbers, puntuation and strip white space
## 7. Remove English stopwords
##
## ####################################################

cleanCorpus <- function(inputCorpus) {

  print("cleanCorpus begins here: ")
  startTime <- proc.time()

  if (!require(tm)) {
    stop("Library tm is missing.")
  }
  
  ## 1. Convert to plain text
  cleanCorpus <- tm_map(inputCorpus, PlainTextDocument)
  
  print("2. Convert to lower case")  
  ## 2. Convert to lower case
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(tolower))
  
  
  print("3. Replace contractions with their full form using qdap dictionary")  
  ## 3. Replace contractions with their full form using qdap dictionary
  ## from https://trinkerrstuff.wordpress.com/my-r-packages/qdap/
  if (!require(qdap)) {
    stop("Library qdap is missing.")
  }
  
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(replace_contraction))
  
  print("4. Remove profanities defined in the banned word list")  
  ## 4. Remove profanities defined in the banned word list
  ## https://www.cs.cmu.edu/~biglou/resources/bad-words.txt
  
  getwd()
  
  profanityWords <- readLines("badwords.txt", encoding = "UTF-8", skipNul=TRUE)
  
  ## 4. remove   profanity  
  cleanCorpus <- tm_map(cleanCorpus, removeWords, profanityWords)
  
  print("5. Remove URLs")  
  ## 5. remove  URLs 
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(removeURL))
  
  ## 6. Remove numbers, puntuation and strip white space
  
  print("6. Remove URLsRemove numbers, puntuation and strip white space")  
  ## Replace non alphabetic characters with spaces
  ## toSpace from onepager.togaware.com/TextMiningO.pdf
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  cleanCorpus <- tm_map(cleanCorpus, toSpace, "[^a-zA-Z]")
  
  
  remove_end_brackets <- function(x) gsub("^[(]|[)]$", " ", x)       # remove any end brackets
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(remove_end_brackets))
  remove_mid_brackets <- function(x) gsub("[(].*?[)]", " ", x)       # remove any middle brackets
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(remove_mid_brackets))

  ## Remove trailing spaces
  ##
  remove_spl <- function(x) gsub("^\\s+|\\s+$", "", x)
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(remove_spl))
  
  
  print("Strip excess white spaces")  
  
  ## Strip excess white spaces
  cleanCorpus <- tm_map(cleanCorpus, stripWhitespace)
  

  stopTime <- proc.time()
  print("Elapsed time of cleanCorpus")
  stopTime - startTime

  ## 7. Remove stopwords
  cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords("english"))
  
}

## Clean Data
library(tm)
library(qdap)

myCleanData <- function(TrainData){

  print("myCleanData begins here: ")
  startTime <- proc.time()
  
  ## Convert data to corpus and inspect a few documents
  trainCorpus <- SimpleCorpus(VectorSource(TrainData))
  inspect(trainCorpus[c(1,length(trainCorpus)%/%2, length(trainCorpus)-20)])
  
  
  trainCorpus <- cleanCorpus(trainCorpus)
  inspect(trainCorpus[c(1,length(trainCorpus)%/%2, length(trainCorpus)-20)])

    ##gc(): this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being used by R.
  gc()

  stopTime <- proc.time()
  print("Elapsed time of myCleanData")
  stopTime - startTime
  
  return(trainCorpus)
  
    
}

## Exploratory analysis 

library(ngram)

getNgram <- function(corpusInput, ngramInput=1){
  startTime <- proc.time()
  
  if (!require(ngram)) {
    stop("Library ngram is missing.")
  }
  
  ## Convert corpus to a string
  corpusStr <- concatenate(corpusInput$content)
  
  corpusNg <- ngram(corpusStr, n=ngramInput)
  x <- get.phrasetable(corpusNg)
  
  x <- x[  with(x, order(-freq) ),]
  
  stopTime <- proc.time()
  print("Elapsed time of getNgram gram")
  stopTime - startTime
  return(x)
}

## 1-gram is a contiguous sequence of single word from the corpus.
ngram1 <- function(trainCorpus) {
  
startTime <- proc.time()
train1Gram <- getNgram(trainCorpus, ngramInput=1)
stopTime <- proc.time()
##saveRDS(train1Gram,v1Gram, t1Gram,  "ngram1.rds")

print("Elapsed time of getNgram 1 gram")
stopTime - startTime

x <- train1Gram[1:50,]

png("bar_ngram1.png")
myplot <- ggplot(x, aes(x= reorder(x$ngram, -x$freq) ,y=x$freq), ) + 
  geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=x$freq), vjust=-0.2) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + xlab("Words") +ylab("Frequency")

print(myplot)
dev.off()
x <- train1Gram[1:100,]
png("wc_ngram1.png")

myplot2 <- wordcloud(x[1:100,]$ngram, x[1:100,]$freq, scale=c(7, 1), 
                     max.words=100, random.order=FALSE,colors=brewer.pal(8, "Dark2"))
print(myplot2)
dev.off()



return (train1Gram)
}


## 2-gram is a contiguous sequence of single word from the corpus.
ngram2 <- function(trainCorpus) {
startTime <- proc.time()
train2Gram <- getNgram(trainCorpus, ngramInput=2)

stopTime <- proc.time()
##saveRDS(train2Gram,v2Gram, t2Gram,  "ngram2.rds")
#print("Elapsed time of getNgram 2 gram")
stopTime - startTime
x <- train2Gram[1:50,]

png("bar_ngram2.png")
myplot <- ggplot(x, aes(x= reorder(x$ngram, -x$freq) ,y=x$freq), ) + 
  geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=x$freq), vjust=-0.2) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + xlab("Words") +ylab("Frequency")

print(myplot)
dev.off()

x <- train2Gram[1:100,]
png("wc_ngram2.png")

myplot2 <- wordcloud(x[1:100,]$ngram, x[1:100,]$freq, scale=c(7, 1), 
                     max.words=100, random.order=FALSE,colors=brewer.pal(8, "Dark2"))

print(myplot2)
dev.off()

return(train2Gram)
}

## 3-gram is a contiguous sequence of single word from the corpus.
ngram3 <- function(trainCorpus) {
startTime <- proc.time()
train3Gram <- getNgram(trainCorpus, ngramInput=3)

stopTime <- proc.time()
print("Elapsed time of getNgram 3 gram")
stopTime - startTime
saveRDS(train3Gram,  "ngram3.rds")
##save(train1Gram, train2Gram, train3Gram, file="trainNGram.rda")
x <- train3Gram[1:50,]

png("bar_ngram3.png")
myplot <- ggplot(x, aes(x= reorder(x$ngram, -x$freq) ,y=x$freq), ) + 
  geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=x$freq), vjust=-0.2) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + xlab("Words") +ylab("Frequency")

print(myplot)
dev.off()

x <- train3Gram[1:100,]
png("wc_ngram3.png")

myplot2 <- wordcloud(x[1:100,]$ngram, x[1:100,]$freq, scale=c(7, 1), 
                     max.words=100, random.order=FALSE,colors=brewer.pal(8, "Dark2"))

print(myplot2)
dev.off()

return(train3Gram)
}

## 4-gram is a contiguous sequence of single word from the corpus.
ngram4 <- function(trainCorpus) {
  startTime <- proc.time()
train4Gram <- getNgram(trainCorpus, ngramInput=4)


stopTime <- proc.time()
#print("Elapsed time of getNgram 4 gram")
stopTime - startTime
saveRDS(train4Gram,  "ngram4.rds")
##saveRDS(train4Gram,v4Gram, t4Gram,  "ngram4.rds")
x <- train4Gram[1:50,]

png("bar_ngram4.png")
myplot <- ggplot(x, aes(x= reorder(x$ngram, -x$freq) ,y=x$freq), ) + 
  geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=x$freq), vjust=-0.2) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + xlab("Words") +ylab("Frequency")

print(myplot)
dev.off()

x <- train4Gram[1:100,]
png("wc_ngram4.png")

myplot2 <- wordcloud(x[1:100,]$ngram, x[1:100,]$freq, scale=c(7, 1), 
                     max.words=100, random.order=FALSE,colors=brewer.pal(8, "Dark2"))

print(myplot2)
dev.off()
return(train4Gram)
}

## 5-gram is a contiguous sequence of single word from the corpus.
ngram5 <- function(trainCorpus) {
  startTime <- proc.time()
  train5Gram <- getNgram(trainCorpus, ngramInput=5)
  
  
  stopTime <- proc.time()
  print("Elapsed time of getNgram 4 gram")
  stopTime - startTime
  saveRDS(train5Gram,  "ngram5.rds")
  ##saveRDS(train4Gram,v4Gram, t4Gram,  "ngram4.rds")
  x <- train5Gram[1:50,]
  
  png("bar_ngram5.png")
  myplot <- ggplot(x, aes(x= reorder(x$ngram, -x$freq) ,y=x$freq), ) + 
    geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=x$freq), vjust=-0.2) + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + xlab("Words") +ylab("Frequency")
  
  print(myplot)
  dev.off()
  
  x <- train5Gram[1:100,]
  png("wc_ngram5.png")
  
  myplot2 <- wordcloud(x[1:100,]$ngram, x[1:100,]$freq, scale=c(7, 1), 
                       max.words=100, random.order=FALSE,colors=brewer.pal(8, "Dark2"))
  
  print(myplot2)
  dev.off()
  return(train5Gram)
}

## cleanNgram: converts a data frame of ngrams to a data table, removes ngrams

cleanNgram <- function(ngramDf) {
  print("cleanNgram begins here: ")
  startTime <- proc.time()
  
  if (!require(stringr)) {
    library(stringr)
    ###stop("Library stringr is missing.")
  }
  
  if (!require(data.table)) {
    library(data.table)
  }
  
  ## Convert the data frame to data table and rename columns
  colnames(ngramDf) <- c("ngram", "freq", "prob")

  stopTime <- proc.time()
  print("Elapsed time of cleanNgram")
  stopTime - startTime
  
  ngramDt <- data.table(ngramDf)
  
}

## split1gram: trims single words and recalculates frequencies and probability:
## p(w1) = count(w1)/count(all w1)
##
## ugramDt = input, a data table of unigrams, c(ngram, freq, prob, freqGT)
## ugramDt = output, a data table with each row is a set of (unigram, word1, 
## freq, prob, freqGT, probGT)
## where freq = original freq, freqGT = freq smoothed by simple GT method.
##
split1Gram <- function(ugramDt) {

  print("split1Gram begins here: ")
  startTime <- proc.time()
  
    if (!require(stringr)) {
    library(stringr)
  }
  
  ## Trim trailing spaces
  ugramDt[, word1 := str_trim(ngram) ]
  setkey(ugramDt, word1)
  
  ## Reset frequencies and calculate words' probability (unsmoothed)
  ugramDt <- ugramDt[, freq := sum(freq), by=c("word1")]
  
  ugramTotalFreq <- sum(ugramDt$freq)
  ugramDt[, prob := freq/ugramTotalFreq]
  
  ## Set key column
  setkey(ugramDt, word1)
  
  ## Reorder the columns in bigrams
  setcolorder(ugramDt, c("ngram", "word1",
                         "freq", "prob"))
  stopTime <- proc.time()
  print("Elapsed time of split1Gram")
  stopTime - startTime
  
  return(ugramDt)
}


split2Gram <- function(bgramDt) {
  if (!require(stringr)) {
    library(stringr)
  }
  
  ## Split the bigram into words
  bgramSplits <- str_split(bgramDt$ngram, boundary("word"))
  bgramDt[, word1 := str_trim( sapply(bgramSplits, function(m) m[1]) ) ]
  bgramDt[, word2 := str_trim(sapply(bgramSplits, function(m) m[2]))]
  
  ## Count instances of word1-word2 and word1 by freq (unsmoothed)
  bgramDt[, count_w1_w2 := sum(freq), by=c("word1", "word2")]
  bgramDt[, count_w1 := sum(freq), by=c("word1")]
  
  ## Calculate p(w2|w1) = count(w1,w2)/count(w1)
  bgramDt[, prob := count_w1_w2/count_w1]
  
  ## Set key columns
  setkey(bgramDt, word1, word2)
  
  ## Reorder the columns in bigrams
  setcolorder(bgramDt, c("ngram", "word1", "word2", 
                         "freq", "prob"))
  return(bgramDt)
}


## split3gram: splits the 3-grams into words, sets words not in the vocabulary 
## to <UNK>, recalculates frequencies and probability:
## p(w3|w1w2) = count(w1,w2,w3)/count(w1,w2)
##
## tgramDt = input, a data table of trigrams, c(ngram, freq, prob, freqGT)
## vocab = input, a list of single words as vocabulary
## tgramDt = output, a data table with each row is a set of (trigram, word1, 
## word2, word3, freq, prob, freqGT, probGT)
## where freq = original freq, freqGT = freq smoothed by simple GT method.
##
split3Gram <- function(tgramDt) {
  if (!require(stringr)) {
    stop("Library stringr is missing.")
  }
  
  ## Split the bigram into words
  tgramSplits <- str_split(tgramDt$ngram, boundary("word"))
  tgramDt[, word1 := str_trim(sapply(tgramSplits, function(m) m[1]))]
  tgramDt[, word2 := str_trim(sapply(tgramSplits, function(m) m[2]))]
  tgramDt[, word3 := str_trim(sapply(tgramSplits, function(m) m[3]))]
  
  
  ## Count instances of word1-word2-word3 and word1-word2 by freq (unsmoothed)
  tgramDt[, count_w1_w2_w3 := sum(freq), by=c("word1", "word2", "word3")]
  tgramDt[, count_w1_w2 := sum(freq), by=c("word1", "word2")]
  
  ## Calculate p(w3|w1w2) = count(w1,w2,w3)/count(w1,w2)
  tgramDt[, prob := count_w1_w2_w3/count_w1_w2]
  
  
  setkey(tgramDt, word1, word2, word3)
  
  ## Reorder the columns in bigrams
  setcolorder(tgramDt, c("ngram", "word1", "word2", "word3",  "freq", "prob"))
  return(tgramDt)
}


split4Gram <- function(tgramDt) {
  if (!require(stringr)) {
    stop("Library stringr is missing.")
  }
  
  ## Split the bigram into words
  tgramSplits <- str_split(tgramDt$ngram, boundary("word"))
  tgramDt[, word1 := str_trim(sapply(tgramSplits, function(m) m[1]))]
  tgramDt[, word2 := str_trim(sapply(tgramSplits, function(m) m[2]))]
  tgramDt[, word3 := str_trim(sapply(tgramSplits, function(m) m[3]))]
  tgramDt[, word4 := str_trim(sapply(tgramSplits, function(m) m[4]))]
  
  
  ## Count instances of word1-word2-word3 and word1-word2 by freq (unsmoothed)
  tgramDt[, count_w1_w2_w3_w4 := sum(freq), by=c("word1", "word2", "word3", "word4")]
  tgramDt[, count_w1_w2_w3 := sum(freq), by=c("word1", "word2", "word3")]
  
  ## Calculate p(w3|w1w2) = count(w1,w2,w3)/count(w1,w2)
  tgramDt[, prob := count_w1_w2_w3_w4/count_w1_w2_w3]
  
  
  setkey(tgramDt, word1, word2, word3)
  
  ## Reorder the columns in bigrams
  setcolorder(tgramDt, c("ngram", "word1", "word2", "word3",  "word4", "freq", "prob"))
  return(tgramDt)
}


split5Gram <- function(tgramDt) {
  if (!require(stringr)) {
    stop("Library stringr is missing.")
  }
  
  ## Split the bigram into words
  tgramSplits <- str_split(tgramDt$ngram, boundary("word"))
  tgramDt[, word1 := str_trim(sapply(tgramSplits, function(m) m[1]))]
  tgramDt[, word2 := str_trim(sapply(tgramSplits, function(m) m[2]))]
  tgramDt[, word3 := str_trim(sapply(tgramSplits, function(m) m[3]))]
  tgramDt[, word4 := str_trim(sapply(tgramSplits, function(m) m[4]))]
  tgramDt[, word5 := str_trim(sapply(tgramSplits, function(m) m[5]))]
  
  
  ## Count instances of word1-word2-word3 and word1-word2 by freq (unsmoothed)
  tgramDt[, count_w1_w2_w3_w4_w5 := sum(freq), by=c("word1", "word2", "word3", "word4", "word5")]
  tgramDt[, count_w1_w2_w3_w4 := sum(freq), by=c("word1", "word2", "word3", "word4")]
  
  ## Calculate p(w3|w1w2) = count(w1,w2,w3)/count(w1,w2)
  tgramDt[, prob := count_w1_w2_w3_w4_w4/count_w1_w2_w3_w4]
  
  
  setkey(tgramDt, word1, word2, word3, word4)
  
  ## Reorder the columns in bigrams
  setcolorder(tgramDt, c("ngram", "word1", "word2", "word3",  "word4", "word5", "freq", "prob"))
  return(tgramDt)
}

#clean the user input data
cleanInput <- function(x) {
  x <- tolower(x)                      # convert to lowercase
  x <- gsub("\\S*[0-9]+\\S*", " ", x)  # remove numbers
  x <- gsub("^[(]|[)]$", " ", x)       # remove any end brackets
  x <- gsub("[(].*?[)]", " ", x)       # remove any middle brackets
  x <- gsub("[[:punct:]]", "", x)      # remove punctuations
  x <- gsub("\\s+"," ",x)              # compress and trim whitespace
  x <- gsub("^\\s+|\\s+$", "", x)
  x <- str_trim(x)
  return(x)
}

check4gram <- function(x, ngram4, getNrows) {
  words <- getLastNwords(x, 3)
  
  match <- subset(ngram4, word1 == words[1] & word2 == words[2] & word3 == words[3])
  ###ngramDt <- ngramDt[freq >= minFreq]
  
  match <- subset(match, select=c(word4, freq))
  
  
  match <- match[order(-match$freq), ] #in frequency decreasing order
  
  head(match)
  
  sumfreq <- sum(match$freq)
  match$freq <- round(match$freq / sumfreq * 100)
  
  colnames(match) <- c("nextword","ngram4.scores")
  
  if (nrow(match) < getNrows) {
    getNrows <- nrow(match)
  }
  match[1:getNrows, ]
}


check3gram <- function(x, ngram3, getNrows) {
  words <- getLastNwords(x, 2)
  #paste(words)
  match <- subset(ngram3, word1 == words[1] & word2 == words[2])
  match <- subset(match, select=c(word3, freq))
  match <- match[order(-match$freq), ]
  head(match)
  sumfreq <- sum(match$freq)
  match$freq <- round(match$freq / sumfreq * 100) 
  colnames(match) <- c("nextword","ngram3.scores")
  if (nrow(match) < getNrows) {
    getNrows <- nrow(match)
  }
  match[1:getNrows, ]
}

check2gram <- function(x, ngram2, getNrows) { 
  words <- getLastNwords(x, 1)
  match <- subset(ngram2, word1 == words[1])
  
  match <- subset(match, select=c(word2, freq))
  match <- match[order(-match$freq), ]
  head(match)
  sumfreq <- sum(match$freq)
  match$freq <- round(match$freq / sumfreq * 100)
  colnames(match) <- c("nextword","ngram2.scores")
  if (nrow(match) < getNrows) {
    getNrows <- nrow(match)
  }
  match[1:getNrows, ]
}


scoreNgrams <- function(x, nrows=30) {

  ##print("check4gram ")  
  x
  ngram4.match <- check4gram(x, ngram4, nrows)

  ##print("check3gram")  
  ngram3.match <- check3gram(x, ngram3, nrows)


    ##print("check2gram")  
  
    ngram2.match <- check2gram(x, ngram2, nrows)

    ##print("check2gram ended")  

    ngram4.match
    ngram3.match
    ngram2.match
    
  merge4n3 <- merge(ngram4.match, ngram3.match, by="nextword", all=TRUE) 
  merge3n2 <- merge(merge4n3, ngram2.match, by="nextword", all=TRUE)
  df <- subset(merge3n2, !is.na(nextword))  # remove any zero-match results
  
  if (nrow(df) > 0) {
    df[is.na(df)] <- 0  # replace all the NAs with 0
    df <- df[order(-df$ngram4.scores, -df$ngram3.scores, -df$ngram2.scores), ]
    
    # Calculate Stupid Back Off score and Rank result.
    alpha <- 0.4
    df$score <- round(
      ifelse(df$ngram4.scores > 0, df$ngram4.scores,
             ifelse(df$ngram3.scores > 0, df$ngram3.scores,
                    ifelse(df$ngram2.scores > 0, df$ngram2.scores,0
                    )
             )
      ),1)
    
    df$score
    df <- df[order(-df$score), ]
    # If result is less than 10, append highest frequenct single word to it.
    if (nrow(df)<10) {
      rest <- 10-nrow(df)
      df <- merge(df, ngram1[1:rest,], by.x=c("nextword","score"), by.y=c("word1","freq"), all=TRUE, sort = FALSE)
    }
  } else {
    print("Match not found, Listing default top 10 words")
    rm(df)
    ##df$nextword <- ngram1[1:10,ngram]
    ##df$score <- ngram1[1:10,freq]
    df <- ngram1[1:10,]
    names(df)
    ##colnames(df) <- c("nextword", "score")
    df$nextword <- df$ngram
    df$score <- df$freq
  }
  ##print("Returning")
  return(df)
}

dataSummary <- function(x) {
  setwd("D:/Work/mystuff/Education/DataScience/DataScienceCapstone/Data")
  
 mydataSummary <-   readRDS("FileDataSummary.rds")

  png("dataSmmary.png")
  print(mydataSummary)
 dev.off()
 
 
}