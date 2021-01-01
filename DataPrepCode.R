# Load the required packages
library(tidytext)
library(tidyverse)
library(stringr)
library(knitr)
library(wordcloud)
library(ngram)


# Download and unzip the Data
if(!file.exists("./Data")){
    dir.create("./Data")
    
    SwiftKeyUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    
    download.file(SwiftKeyUrl, destfile = "./Data/Coursera-SwiftKey.zip")
    
    #UNZIP DATA
    unzip("./Data/Coursera-SwiftKey.zip", exdir = "./Data")
}

# Load and Read the Data
twitterFile <- "./Data/final/en_US/en_US.twitter.txt"
blogsFile <- "./Data/final/en_US/en_US.blogs.txt"
newsFile <- "./Data/final/en_US/en_US.news.txt"

twitterData <- readLines(twitterFile, skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
blogsData <- readLines(blogsFile, skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
newsData <- readLines(newsFile, skipNul = TRUE, warn = FALSE, encoding = "UTF-8")


# Create Dataframes for the Data
twitterDF <- tibble(Texts = twitterData)
blogsDF <- tibble(Texts = blogsData)
newsDF <- tibble(Texts = newsData)


# Sampling the Data
set.seed(2020)
sampleSize <- 0.05

twitterSample <- twitterDF %>%
    sample_n(nrow(twitterDF) * sampleSize)
blogsSample <- blogsDF %>%
    sample_n(nrow(blogsDF) * sampleSize)
newsSample <- newsDF %>%
    sample_n(nrow(newsDF) * sampleSize)


# Create tidy Sample Data
dataSample <- bind_rows(
    mutate(twitterSample, sourceType = "Twitter"),
    mutate(blogsSample,  sourceType = "Blogs"),
    mutate(newsSample, sourceType = "News"))

dataSample$sourceType <- as.factor(dataSample$sourceType)

# Clear the un-neccessary data variables
rm(list = c("twitterSample", "blogsSample", "newsSample", "twitterDF", 
            "blogsDF", "newsDF"))

# Clean the Sample Data

# Cleaning Stop - Words
data("stop_words")

# Remove Offensive Words
offeWordFile <- "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"

download.file(offeWordFile, destfile = "./Data/offensiveWords.txt")
offeWordFile <- read.csv("./Data/offensiveWords.txt", col.names = "Words", colClasses = "character")

# Create filters for: non-alphanumeric's, url's, repeated letters(+3x)
replaceReg <- "[^[:alpha:][:space:]]*"
replaceURL <- "http[^[:space:]]*"
replaceRepLet <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"

# Clean the sampleData. Cleaning is separted from tidying so `unnest_tokens` function can be used for words, and ngrams.
cleanDataSample <-  dataSample %>%
    mutate(Texts = str_replace_all(Texts, replaceReg, "")) %>%
    mutate(Texts = str_replace_all(Texts, replaceURL, "")) %>%
    mutate(Texts = str_replace_all(Texts, replaceRepLet, "")) %>%
    mutate(Texts = str_squish(Texts)) %>%
    mutate(Texts = str_trim(Texts))

#Remove unnecessary Data
rm(list = c("dataSample"))

# Generate Ngrams
# Unigrams
uniGramData <- cleanDataSample %>%
    unnest_tokens(Words, Texts) %>%
    anti_join(offeWordFile, by = c("Words" = "Words")) %>%
    anti_join(stop_words, by = c("Words" = "word"))

# Bigrams
biGramData <- cleanDataSample %>%
    unnest_tokens(bigram, Texts, token = "ngrams", n = 2)

# Trigrams
triGramData <- cleanDataSample %>%
    unnest_tokens(trigram, Texts, token = "ngrams", n = 3)

# Quadragrams
quadraGramData <- cleanDataSample %>%
    unnest_tokens(quadragram, Texts, token = "ngrams", n = 4)

# Pentagrams
pentaGramData <- cleanDataSample %>%
    unnest_tokens(pentagram, Texts, token = "ngrams", n = 5)

# Hexagrams
hexaGramData <- cleanDataSample %>%
    unnest_tokens(hexagram, Texts, token = "ngrams", n = 6)

# Reduce n-grams files by frequency
# Unigrams
unigramFinal <- uniGramData %>%
    count(Words) %>%
    filter(n > 5) %>%
    arrange(desc(n))
# Bigrams
bigramFinal <- biGramData %>%
    count(bigram) %>%
    filter(n > 5) %>%
    arrange(desc(n))
# Trigrams
trigramFinal <- triGramData %>%
    count(trigram) %>%
    filter(n > 5) %>%
    arrange(desc(n))
# Quadragrams
quadragramFinal <- quadraGramData %>%
    count(quadragram) %>%
    filter(n > 5) %>%
    arrange(desc(n))
# Pentagrams
pentagramFinal <- pentaGramData %>%
    count(pentagram) %>%
    filter(n > 5) %>%
    arrange(desc(n))
# Hexagrams
hexagramFinal <- hexaGramData %>%
    count(hexagram) %>%
    filter(n > 5) %>%
    arrange(desc(n))

#Removing unnecessary data
rm(list = c("biGramData", "triGramData", "quadraGramData", "pentaGramData", "hexaGramData"))

# Separate words
# NgramWords
uniWords <- unigramFinal %>%
    separate(Words, c("FirstWord"), sep = " ")

biWords <- bigramFinal %>%
    separate(bigram, c("FirstWord", "SecondWord"), sep = " ")

triWords <- trigramFinal %>%
    separate(trigram, 
             c("FirstWord", "SecondWord", "ThirdWord"), sep = " ")

quadraWords <- quadragramFinal %>%
    separate(quadragram, 
             c("FirstWord", "SecondWord", "ThirdWord", "FourthWord"), sep = " ")

pentaWords <- pentagramFinal %>%
    separate(pentagram, 
             c("FirstWord", "SecondWord", "ThirdWord", "FourthWord", "FifthWord"), sep = " ")

hexaWords <- hexagramFinal %>%
    separate(hexagram, 
             c("FirstWord", "SecondWord", "ThirdWord", "FourthWord", "FifthWord", "SixthWord"), sep = " ")

# Save the data for the Next Word Predictor Shiny App
dir.create("./finalNgramData", showWarnings = FALSE)

saveRDS(uniWords, "./finalNgramData/UniWords.rds")
saveRDS(biWords, "./finalNgramData/BiWords.rds")
saveRDS(triWords, "./finalNgramData/TriWords.rds")
saveRDS(quadraWords,"./finalNgramData/QuadraWords.rds")
saveRDS(pentaWords,"./finalNgramData/PentaWords.rds")
saveRDS(hexaWords,"./finalNgramData/hexaWords.rds")
