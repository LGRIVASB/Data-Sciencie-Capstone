### Data Science Capstone : Final Project App: Next Word Prediction

##Package

library(tidyverse)
library(stringr)
library(shiny)

## Loading Train Data

BiWords <- readRDS("./finalNgramData/BiWords.rds")
TriWords  <- readRDS("./finalNgramData/TriWords.rds")
QuadraWords <- readRDS("./finalNgramData/QuadraWords.rds")
PentaWords <- readRDS("./finalNgramData/PentaWords.rds")
HexaWords <- readRDS("./finalNgramData/hexaWords.rds")

## Create Ngram Functions

biFunction <- function(inputWords){
    numWords <- length(inputWords)
    answer <- filter(BiWords, FirstWord == inputWords[numWords]) %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(SecondWord) %>%
        as.character()
    
    ifelse(answer == "character(0)", "?", return(answer))
}

triFunction <- function(inputWords){
    numWords <- length(inputWords)
    answer <- filter(TriWords, FirstWord == inputWords[numWords - 1], SecondWord == inputWords[numWords])  %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(ThirdWord) %>%
        as.character()
    
    ifelse(answer == "character(0)", biFunction(inputWords), return(answer))
}

quadraFunction <- function(inputWords){
    numWords <- length(inputWords)
    answer <- filter(QuadraWords,
                     FirstWord == inputWords[numWords - 2],
                     SecondWord == inputWords[numWords - 1],
                     ThirdWord == inputWords[numWords])  %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(FourthWord) %>%
        as.character()
    
    ifelse(answer == "character(0)", triFunction(inputWords), return(answer))
}

pentaFunction <- function(inputWords){
    numWords <- length(inputWords)
    answer <- filter(PentaWords,
                     FirstWord == inputWords[numWords - 3],
                     SecondWord == inputWords[numWords - 2],
                     ThirdWord == inputWords[numWords - 1],
                     FourthWord == inputWords[numWords])  %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(FifthWord) %>%
        as.character()
    
    ifelse(answer == "character(0)", quadraFunction(inputWords), return(answer))
}

hexaFunction <- function(inputWords){
    numWords <- length(inputWords)
    answer <- filter(HexaWords,
                     FirstWord == inputWords[numWords - 4],
                     SecondWord == inputWords[numWords - 3],
                     ThirdWord == inputWords[numWords - 2],
                     FourthWord == inputWords[numWords - 1],
                     FifthWord == inputWords[numWords])  %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(SixthWord) %>%
        as.character()
    
    ifelse(answer == "character(0)", pentaFunction(inputWords), return(answer))
}

#Create User Input and Data Cleaning Function; Calls the matching functions

NGramFunction <- function(inputText){
    # Create Dataframe
    inputText <- tibble(text = inputText)
    # Clean the Input
    replaceReg <- "[^[:alpha:][:space:]]*"
    inputText <- inputText %>%
        mutate(text = str_replace_all(text, replaceReg, ""))
    # Count number of words, separate and apply lower case
    wordCount <- str_count(inputText, boundary("word"))
    wordsInput <- unlist(str_split(inputText, boundary("word")))
    wordsInput <- tolower(wordsInput)
    # Matching functions
    wordAnswer <- if(wordCount == 0){
        "Please enter a phrase in the given left text box!!!"
    }else if(wordCount == 1){
        biFunction(wordsInput)
    }else if(wordCount == 2){
        triFunction(wordsInput)
    }else if(wordCount == 3){
        quadraFunction(wordsInput)
    }else if(wordCount == 4){
        pentaFunction(wordsInput)
    }else{
        hexaFunction(wordsInput)
    }
    # Return Answer    
    if(wordAnswer == "?"){
        wordAnswer = "The application not found the next expected word. This is caused by the limited size of data used to train the prediction model. Try another word or phrase!!!!" 
    }
    return(wordAnswer)
}

shinyServer(function(input, output){
    output$prediction <- renderPrint({
        predictWord <- NGramFunction(input$inputString)
        predictWord
    })
    
    output$text1 <- renderText({
        if(input$inputString != ""){
            predictWord <- NGramFunction(input$inputString)
            paste(input$inputString, predictWord, sep = " ")
        }
    });
})