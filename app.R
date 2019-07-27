#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  
  titlePanel("Predict Next Word"),
  
  tabsetPanel( type='tab',
              tabPanel("App Main Page",
                       sidebarLayout(
                         
                         sidebarPanel(
                           helpText("Application Instruction:",br(),
                                    "Type some text into the text box under the \"Text Input your phrase here\" heading"),
                           
                           textInput('userInput',label="Input your phrase here:",value=""),
                           #actionButton('goButton',"Guess!"),
                           
                           br(),
                           helpText("Note:",br(),
                                    "The following predicted word will show up automatically as you input.")),
                         
                         mainPanel(
                           h4("Here are the top 10 predictions:"),
                           ##verbatimTextOutput('guess')
                           verbatimTextOutput('guess')
                           ##  cat(paste(text1, text2, text3, sep="\n"))
                           ##textOutput('guess')
                         )
                       )
              ),
              tabPanel( "Data Summary", 
                        h4("Introduction"),
                        p("Application: Text Prediction Application."),
                        p("The purpose of this project is to build a predictive text models. When someone types: 'I went to the' "),
                        p("the keyboard presents then options for what the next word might be. "),
                        h4("Text Prediction Model"),
                        p("Prediction model will be based on backoff model in NLP. I have used 4-grams to calculate the probability of a word in text. Model will go back to a n-1 gram level to calculate the probabilities of finding a word with prob=0."),

                       h3("Data Details"),
                       imageOutput("datasummary")
                       ),
              tabPanel("Exploratory Analysis", 
                       p("Exploratory Analysis of the data involves understanding the distribution of words and relationship between the words in the corpora.
  - Calculated the frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data"),
                       h3("Unigram bar data analysis"),
                       p("An n-gram consisting of a single item from a sequence of words in a text file"),
                       p("Following displays the bar and word cloud of frequency distribution of a single words"),
                       
                       imageOutput("barngram1"),
                       h3("Unigram wordcloud data analysis"),
                       imageOutput("wcngram1"),
                       
                       h3("Bigram bar data analysis"),
                       p("An n-gram consisting of a two item from a sequence of words in a text file"),
                       p("Following displays the bar and word cloud of frequency distribution of a two words"),
                       imageOutput("barngram2"),
                       h3("Bigram wordcloud data analysis"),
                       imageOutput("wcngram2"),
                       
                       h3("Triigram bar data analysis"),
                       p("An n-gram consisting of a three item from a sequence of words in a text file"),
                       p("Following displays the bar and word cloud of frequency distribution of a three words"),
                       imageOutput("barngram3"),
                       h3("Trigram wordcloud data analysis"),
                       imageOutput("wcngram3"),
                       h3("Quadgram bar data analysis"),
                       p("An n-gram consisting of a four item from a sequence of words in a text file"),
                       p("Following displays the bar and word cloud of frequency distribution of a four words"),
                       imageOutput("barngram4"),
                       h3("Quadgram wordcloud data analysis"),
                       imageOutput("wcngram4"),
                       h3("Pentagram bar data analysis"),
                       p("An n-gram consisting of a five item from a sequence of words in a text file"),
                       p("Following displays the bar and word cloud of frequency distribution of a five words"),
                       
                       imageOutput("barngram5"),
                       h3("Bigram wordcloud data analysis"),
                       imageOutput("wcngram5"),
                       
                        h5("For more information of exploratory analysis of dateset, please refer to my milestone document."),
                       p(" "),
                       p(""),
                       a(p("LINK"), href="http://rpubs.com/ajay_jalan/508765")
  ),
  hr(),
  h4("Author: Ajay Aggarwal :-)",
     p(""),
     p(""),
     a(p("Github Repo."), href="https://github.com/ajay-aggarwal01/capStoneProject")
  )  
  
)     
)



isValid <- function(input) {
  if (length(input) == 0) FALSE
  else if (length(input[grep("^\\W+$", input, perl = TRUE)])) FALSE
  else if (length(input[grep("^\\d+$", input, perl = TRUE)])) FALSE
  else if (length(input) == 1 && input[1] == "") FALSE
  else if (length(input) == 1 && input[1] != "") TRUE
  else FALSE
}


library(shiny)
##setwd("D:/Work/mystuff/Education/DataScience/DataScienceCapstone")

source("W3_Assignment.R")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  print("Request received!")
  
  reactiveInputHandler1 <- reactive( {
    if (isValid(input$inputText)) return(as.character(input$inputText))
    else return("<Please use a valid input>")
  } )
  
  output$datasummary <- renderImage( {
    return(list(
      src = "dataSummary.png",
      contentType = "image/png",
      width = 600,
      height = 400,
      alt = "Face"
    ))
  }, deleteFile = FALSE)

  output$barngram1 <- renderImage( {
    return(list(
      src = "bar_ngram1.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  

  output$wcngram1 <- renderImage( {
    return(list(
      src = "wc_ngram1.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$barngram2 <- renderImage( {
    return(list(
      src = "bar_ngram2.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  output$wcngram2 <- renderImage( {
    return(list(
      src = "wc_ngram2.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$barngram3 <- renderImage( {
    return(list(
      src = "bar_ngram3.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  output$wcngram3 <- renderImage( {
    return(list(
      src = "wc_ngram3.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$barngram4 <- renderImage( {
    return(list(
      src = "bar_ngram4.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  output$wcngram4 <- renderImage( {
    return(list(
      src = "wc_ngram4.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  

  output$barngram5 <- renderImage( {
    return(list(
      src = "bar_ngram5.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  output$wcngram5 <- renderImage( {
    return(list(
      src = "wc_ngram5.png",
      contentType = "image/png",
      width = 200,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  #Display user's input
  output$otext<- renderText(reactiveInputHandler1())
  
  output$otext<-renderPrint( {as.character(input$input_text)})
  dataInput <- reactive(  {
    as.character(nextWord(input$userInput)$nextword)[1:10]
    ##sizeL <- length(x)
    ##print(sizeL)
    ##resultString <- ""
    ##currentString <-""
    
    ##for(i in sizeL){
      ##currentString <- paste(i,x[i], sep = "       ")
      ##resultString <- paste(resultString, currentString, sep = "\n")
    ##}
    ##return(resultString)
    
    
  } )
  
  output$guess <- renderPrint({dataInput() }  )
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)


# Application title
##titlePanel("Old Faithful Geyser Data"),

# Sidebar with a slider input for number of bins 
##sidebarLayout(
  ##sidebarPanel(
    ##sliderInput("bins",
      ##          "Number of bins:",
        ##        min = 1,
          ##      max = 50,
            ##    value = 30)
  ## ),
  
  # Show a plot of the generated distribution
  ## mainPanel(
    ##plotOutput("distPlot")
  ##)
##)

##output$distPlot <- renderPlot({
  # generate bins based on input$bins from ui.R
  ##x    <- faithful[, 2] 
  ##bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  ##hist(x, breaks = bins, col = 'darkgray', border = 'white')
##})
