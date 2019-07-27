
##This is R file contains functions required by WordPredictApp.
##setwd("~/online courses/JHU-DataScience/10 Capstone Project/WordPredictApp")


##setwd("D:/Work/mystuff/Education/DataScience/DataScienceCapstone")
source("wordPredictFunctionsLib.R")


if ("shiny" %in% rownames(installed.packages()) == FALSE){
  install.packages('shiny')
}
if ("ggplot2" %in% rownames(installed.packages()) == FALSE){
  install.packages('ggplot2')
}
if ("BH" %in% rownames(installed.packages()) == FALSE){
  install.packages('BH')
}
if ("NLP" %in% rownames(installed.packages()) == FALSE){
  install.packages('NLP')
}
library(shiny)
library(ggplot2)
library(BH)
library(NLP)
library(stringr)

##setwd("D:/Work/mystuff/Education/DataScience/DataScienceCapstone/Data")

#Load the ngram dataframe
ngram4 <- readRDS("ngram4dt.rds")
ngram3 <- readRDS("ngram3dt.rds")
ngram2 <- readRDS("ngram2dt.rds")
ngram1 <- readRDS("ngram1dt.rds")

ngram1 <- ngram1[  with(ngram1, order(-freq) ),]
ngram2 <- ngram2[  with(ngram2, order(-freq) ),]
ngram3 <- ngram3[  with(ngram3, order(-freq) ),]
ngram4 <- ngram4[  with(ngram4, order(-freq) ),]


nextWord <- function(inputText, nrows=30) {
  
  #clean the user input data
  myText <- cleanInput(inputText)
  myText <- getLastNwords(myText, 3, seperator = " ") 
  
  df <- scoreNgrams(myText)
  return(df)
  }

