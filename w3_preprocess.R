##
## Read Data from blog, news and twitter files and sample it based on sampling factor
##

setwd("D:/Work/mystuff/Education/DataScience/DataScienceCapstone")

source("wordPredictFunctionsLib.R")
wd <- "D:/Work/mystuff/Education/DataScience/DataScienceCapstone/Data"
sf <- 0.08

library(stringi)

## 47.9 MB data was created here
fileName <- "mySample.txt"
startTime <- proc.time()
mySample <- mySampleData(wd, sf, fileName)

stopTime <- proc.time()
print("Elapsed time:")
stopTime - startTime


library(caret)

trainP <- 0.8
validateP <- 0.1
testP  <- 0.1

## Function creates followiung files: 
## TrainData.rds
## TestData.rds
## ValidateData.rds

startTime <- proc.time()

## Train Data size is 38.2mb

TrainData <- myTrainData(mySample,trainP, validateP, testP)

stopTime <- proc.time()
print("Elapsed time:")

stopTime - startTime

## "Elapsed time:"
## user  system elapsed 
## 4.87    0.11    5.01 

## We do not need mySample now
rm(mySample)


library(tm)
library(qdap)

## Convert data to corpus and inspect a few documents
startTime <- proc.time()
trainCorpus <- myCleanData(TrainData) 

rm(TrainData)
stopTime <- proc.time()
print("Elapsed time:")
stopTime - startTime

## "Elapsed time:"
## user  system elapsed 
## 177.71    0.36  178.98 

## Exploratory analysis 

library(ngram)
library("wordcloud")

## 1-gram is a contiguous sequence of single word from the corpus.
startTime <- proc.time()
train1Gram <- ngram1(trainCorpus)

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

saveRDS(train1Gram,  "ngram1.rds")
rm(train1Gram)
stopTime <- proc.time()
print("Elapsed time:")
stopTime - startTime

## "Elapsed time:"
## user  system elapsed 
## 169.73    0.36  170.49

## 2-gram is a contiguous sequence of single word from the corpus.
startTime <- proc.time()
train2Gram <- ngram2(trainCorpus) 

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

saveRDS(train2Gram,  "ngram2.rds")
rm(train2Gram)
stopTime <- proc.time()
print("Elapsed time:")
stopTime - startTime

## "Elapsed time:"
## user  system elapsed 
## 12.00    0.49   12.54 

## 3-gram is a contiguous sequence of single word from the corpus.
startTime <- proc.time()
train3Gram <- ngram3(trainCorpus) 

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

saveRDS(train3Gram,  "ngram3.rds")
rm(train3Gram)
stopTime <- proc.time()
print("Elapsed time:")
stopTime - startTime

## "Elapsed time:"
## user  system elapsed 
## 25.37    0.64   26.19 

## 4-gram is a contiguous sequence of single word from the corpus.
startTime <- proc.time()
train4Gram <- ngram4(trainCorpus) 
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

saveRDS(train4Gram,  "ngram4.rds")
rm(train4Gram)
stopTime <- proc.time()
print("Elapsed time:")
stopTime - startTime

## "Elapsed time:"
## user  system elapsed 
## 25.90    0.55   26.55 

## 5-gram is a contiguous sequence of single word from the corpus.
startTime <- proc.time()
train5Gram <- ngram5(trainCorpus) 
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

saveRDS(train5Gram,  "ngram5.rds")
rm(train5Gram)
stopTime <- proc.time()
print("Elapsed time:")
stopTime - startTime

## "Elapsed time:"
## user  system elapsed 
## 30.20    0.72   31.06 

#Load the ngram dataframe
ngram5 <- readRDS("ngram5.rds")
ngram4 <- readRDS("ngram4.rds")
ngram3 <- readRDS("ngram3.rds")
ngram2 <- readRDS("ngram2.rds")
ngram1 <- readRDS("ngram1.rds")

ngram1 <- ngram1[  with(ngram1, order(-freq) ),]
ngram2 <- ngram2[  with(ngram2, order(-freq) ),]
ngram3 <- ngram3[  with(ngram3, order(-freq) ),]
ngram4 <- ngram4[  with(ngram4, order(-freq) ),]
ngram5 <- ngram5[  with(ngram5, order(-freq) ),]

## Conver data frame into data table
ngram1dt <- cleanNgram(ngram1)
ngram2dt <- cleanNgram(ngram2)
ngram3dt <- cleanNgram(ngram3)
ngram4dt <- cleanNgram(ngram4)
ngram5dt <- cleanNgram(ngram5)

rm(ngram1)
rm(ngram2)
rm(ngram3)
rm(ngram4)
rm(ngram5)

ngram1dt <- ngram1dt[  with(ngram1dt, order(-freq) ),]
ngram2dt <-ngram2dt[  with(ngram2dt, order(-freq) ),]
ngram3dt <-ngram3dt[  with(ngram3dt, order(-freq) ),]
ngram4dt <-ngram4dt[  with(ngram4dt, order(-freq) ),]
ngram5dt <-ngram4dt[  with(ngram5dt, order(-freq) ),]

ngram1dt <- split1Gram(ngram1dt) 
ngram1dt <- ngram1dt[  with(ngram1dt, order(-freq) ),]

ngram2dt <- split2Gram(ngram2dt) 
ngram2dt <- ngram2dt[  with(ngram2dt, order(-freq) ),]

ngram3dt <- split3Gram(ngram3dt) 
ngram3dt <- ngram3dt[  with(ngram3dt, order(-freq) ),]

ngram4dt <- split4Gram(ngram4dt) 
ngram4dt <- ngram4dt[  with(ngram4dt, order(-freq) ),]

saveRDS(ngram4dt,  "ngram4dt.rds")
saveRDS(ngram3dt,  "ngram3dt.rds")
saveRDS(ngram2dt,  "ngram2dt.rds")
saveRDS(ngram1dt,  "ngram1dt.rds")

write.table(ngram4dt, file="ngram4dt.txt" )
write.table(ngram3dt, file="ngram3dt.txt" )
write.table(ngram2dt, file="ngram2dt.txt" )
write.table(ngram1dt, file="ngram1dt.txt" )
                                                
rm(ngram1dt)
rm(ngram2dt)
rm(ngram3dt)
rm(ngram4dt)
rm(trainCorpus)