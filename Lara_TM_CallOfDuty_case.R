#' Title: Case 2 Call of Duty
#' Purpose: Case Study
#' Author: Kathleen Lara
#' email: kathleencastrolara@gmail.com
#' Date: January 23, 2021

# Set the working directory
setwd("~/Desktop/hult_NLP_student/cases/Call of Duty E-Sport/teamTimeline")

#Libraries
library(tm)
library(qdap)
library(lexicon)
library(ggplot2)
library(plyr)
library(dplyr)
library(fst)
library(pbapply)
library(mgsub)
library(tidytext)
library(reshape2)
library(wordcloud)
library(viridisLite)
library(radarchart)

#Options and Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')


tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

stops <- c(stopwords('SMART'),'we','us', 'today', 
           'been', 'are','themselves', 'team','game','excited','into','its','country'
           , 'on','the','dallasempire','cdl','call', 'duty', 'of', 'win', 'lose', 'amp','ct','day','live',
           'pm','am','dallas','us','series','...','link','oncoming','tomorrow','yesterday','seattle', 'SEA', 'DAL') 

# Exploring the Dataset
teamDF <- read_fst('student_TeamTimelines.fst') 
head(teamDF)

#Count number of tweets per team and look at how many tweets the winning and losing team from last season has
ggplot(data = teamDF, aes(x = screen_name)) +
  geom_bar(stat = "count") + 
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5))


# Sentiment Analysis
emoji <- read.csv('~/Desktop/hult_NLP_student/cases/Call of Duty E-Sport/emojis.csv')
teamDF$text <- pbsapply(as.character(teamDF$text), mgsub, emoji$emoji, emoji$name)

# Clean and Organize into VCorpus
txt <- VCorpus(VectorSource(teamDF$text))
txt <- cleanCorpus(txt, stops)
content(txt[[1]])

# Extracting the clean tweets and pulling out the needed collumn for the analysis
cleanTweets <- data.frame(document = seq_along(teamDF$text), #simple id order
                         status_id = teamDF$status_id, # keep track of posts
                         screen_name = teamDF$screen_name,
                         text = unlist(sapply(txt, `[`, "content")),stringsAsFactors=F)

# Polarity & append each tweet score
pol <- polarity(cleanTweets$text,cleanTweets$screen_name)
pol$group

cleanTweets$polarityValue <- pol$all$polarity
cleanTweets$polarityValue[is.na(cleanTweets$polarityValue)] <- 0

# Classifying the polarity scores
cleanTweets$polarityClass <- ifelse(cleanTweets$polarityValue>0, 'positive',
                                   ifelse(cleanTweets$polarityValue<0, 'negative', 'neutral'))


# Assigning an emotion to each tweet
txtDTM   <- DocumentTermMatrix(VCorpus(VectorSource(cleanTweets$text)))
tidyCorp <- tidy(txtDTM)
tidyCorp
dim(tidyCorp)

nrc     <- get_sentiments(lexicon = c("nrc"))
nrcSent <- inner_join(tidyCorp,nrc, by=c('term' = 'word'))
nrcSent


# Grouping the document and selecting the most numerous 
grpSent <- nrcSent %>% group_by(document, sentiment) %>% summarise(n = sum(count))
grpSent$document <- as.numeric(as.character(grpSent$document))
grpSent

# Casting to wide format
wideSent <- dcast(grpSent, document~sentiment,fun.aggregate = sum,value.var = "n")
head(wideSent) 
wideSent[grep('\\b100\\b',wideSent$document),] 


# Drop positive/negative and get maximum column
wideSent <- wideSent[,-c(7,8)]
wideSent$maxEmotion <- ifelse(rowSums(wideSent[,2:ncol(wideSent)])>0,
                              names(wideSent)[2:ncol(wideSent)][max.col(wideSent[,2:ncol(wideSent)])],
                              'noEmotion')
head(wideSent)

# Some posts are neutral so you cant cbind, instead left_join
cleanTweets <- left_join(cleanTweets, wideSent, by = c('document'='document'))
cleanTweets$maxEmotion[is.na(cleanTweets$maxEmotion)] <- 'noEmotion' #NA introduced from join on docs that had no emotion

# Clean DF with polarity, and emotional sentiment
head(cleanTweets)

# Subsetting and getting WFM for screen_names (winner and loser last season)
DAL_Winner <- subset(cleanTweets$text, cleanTweets$screen_name=='DallasEmpire', header=TRUE)
SEA_Loser <- subset(cleanTweets$text, cleanTweets$screen_name=='SeattleSurge', header=TRUE)

plot(freq_terms(DAL_Winner, top=35, at.least=2, stopwords = stops))
plot(freq_terms(SEA_Loser, top=35, at.least=2, stopwords = stops))


# Comparison cloud of the emotions using for loops
polarityLst <- list()
for(i in 1:length(unique(cleanTweets$polarityClass))){
  x <- subset(cleanTweets$text, cleanTweets$polarityClass == unique(cleanTweets$polarityClass)[i])
  x <- paste(x, collapse = ' ')
  polarityLst[[unique(cleanTweets$polarityClass)[i]]] <- x
}

# Using the list
allPolarityClasses <- do.call(rbind, polarityLst)
allPolarityClasses <- VCorpus(VectorSource(allPolarityClasses))
allPolarityClasses <- TermDocumentMatrix(cleanCorpus(allPolarityClasses, stops))
allPolarityClasses <- as.matrix(allPolarityClasses)


# Add the names from the list, get the order right!
colnames(allPolarityClasses) <- names(polarityLst)

# Make comparison cloud
comparison.cloud(allPolarityClasses, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=1,
                 colors=brewer.pal(ncol(allPolarityClasses),"Dark2"),
                 scale=c(3,0.1))
dev.off()

# Repeat for the max emotion
emotionLst <- list()
for(i in 1:length(unique(cleanTweets$maxEmotion))){
  x <- subset(cleanTweets$text, cleanTweets$maxEmotion == unique(cleanTweets$maxEmotion)[i])
  x <- paste(x, collapse = ' ')
  emotionLst[[unique(cleanTweets$maxEmotion)[i]]] <- x
}

# Using the list
allEmotionClasses <- do.call(rbind, emotionLst)
allEmotionClasses <- VCorpus(VectorSource(allEmotionClasses))
allEmotionClasses <- TermDocumentMatrix(allEmotionClasses)
allEmotionClasses <- as.matrix(allEmotionClasses)

colnames(allEmotionClasses) <- names(emotionLst)

#Comparison Cloud
comparison.cloud(allEmotionClasses, 
                 max.words=500, 
                 random.order=FALSE,
                 title.size=1,
                 colors=viridis(10),
                 scale=c(3,0.1))


# After doing the Sentiment Analysis we want to now start looking for unique opportunities
# We need major partners or sponsors who can back up our new team (to help us promote them)


# Clean and Organize
tweetsDF <- read_fst('student_TeamTimelines.fst') 

DAL_WinnerDF <- subset(tweetsDF, screen_name == 'DallasEmpire')
SEA_LoserDF <- subset(tweetsDF, screen_name == 'SeattleSurge')



#Clean and Organize tweets of Dallas and Seattle into a Vcorpus
DAL_Corpus <- VCorpus(VectorSource(DAL_WinnerDF$text))
SEA_Corpus <- VCorpus(VectorSource(SEA_LoserDF$text))
DAL_Corpus <- cleanCorpus(DAL_Corpus, stops)
SEA_Corpus <- cleanCorpus(SEA_Corpus, stops)
DAL_DTM  <- DocumentTermMatrix(DAL_Corpus)
SEA_DTM  <- DocumentTermMatrix(SEA_Corpus)
DAL_DTMm <- as.matrix(DAL_DTM)
SEA_DTMm <- as.matrix(SEA_DTM)



DAL_DTM[,1:10]
dim(DAL_DTM)

SEA_DTM[,1:10]
dim(SEA_DTM)

# Examine Tidy & Compare Dallas
DAL_tmp     <- as.DocumentTermMatrix(DAL_DTM, weighting = weightTf ) 
DAL_tidyCorp <- tidy(DAL_tmp)
DAL_tidyCorp
dim(DAL_tidyCorp)

# Examine Tidy & Compare Seattle
SEAL_tmp     <- as.DocumentTermMatrix(SEA_DTM, weighting = weightTf ) 
SEA_tidyCorp <- tidy(SEAL_tmp)
SEA_tidyCorp
dim(SEA_tidyCorp)

# Get bing lexicon
# "afinn", "bing", "nrc", "loughran"
bing <- get_sentiments(lexicon = c("bing"))
head(bing)


# Perform Inner Join
DAL_bingSent <- inner_join(DAL_tidyCorp, bing, by=c('term' = 'word'))
DAL_bingSent


SEA_bingSent <- inner_join(SEA_tidyCorp, bing, by=c('term' = 'word'))
SEA_bingSent


# Quick Analysis
table(DAL_bingSent$sentiment, DAL_bingSent$count)
aggregate(count~sentiment,DAL_bingSent, sum)

table(SEA_bingSent$sentiment, SEA_bingSent$count)
aggregate(count~sentiment,SEA_bingSent, sum)

# Compare original with qdap::Polarity
polarity(DAL_WinnerDF$text)
polarity(SEA_LoserDF$text)


# Get afinn lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)


# Perform Inner Join
DAL_afinnSent <- inner_join(DAL_tidyCorp,afinn, by=c('term' = 'word'))
DAL_afinnSent

SEA_afinnSent <- inner_join(SEA_tidyCorp,afinn, by=c('term' = 'word'))
SEA_afinnSent

#  Analysis of DallasEmpire - Winner
DAL_teams <- DAL_WinnerDF$text
DAL_teamsWords <- data.frame(word = unlist(strsplit(DAL_teams,' ')))
DAL_teamsWords$word <- tolower(DAL_teamsWords$word )
DAL_teamsWords <- left_join(DAL_teamsWords,afinn, by=c('word' = 'word'))
DAL_teamsWords[is.na(DAL_teamsWords$value),2] <- 0
plot(DAL_teamsWords$value, type="l", main="Dallas Empire Timeline") 

#  Analysis of SeattleSurge - Losing team
SEA_team <- SEA_LoserDF$text
SEA_teamWords <- data.frame(word = unlist(strsplit(SEA_team,' ')))
SEA_teamWords$word <- tolower(SEA_teamWords$word )
SEA_teamWords <- left_join(SEA_teamWords,afinn, by=c('word' = 'word'))
SEA_teamWords[is.na(SEA_teamWords$value),2] <- 0
plot(SEA_teamWords$value, type="l", main="Seattle Surge Timeline") 

