library(rtweet)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)

# Twitter API Credentials
api_key <- 'PdyB932vFQA76ZF3gOsqQ8bwv'
api_secret <- 'ijBrl9eCo2AbxFZEdrf5sOyzeEoXiPRzObxxiX9gmkcnnwMb8m'
access_token <- '941640545671065600-7865usfa7VWQKSCWHo8K9HDAw7PCwoS'
access_token_secret <- 'hdFozgFPilbSOMnm7rJL4hWQdyNQwvSgbIu8m1BKKinon'

create_token(app='Tweet Analysis', api_key, api_secret, access_token, access_token_secret)

# TWEETS COLLECTED ON 04:30 PM 03/12/2022 EST
tweets_google <- search_tweets('$GOOGL', n=3000, lang='en')
tweets_google_df <- as.data.frame(tweets_google)
tweets_google_df <- apply(tweets_google_df,2,as.character)
write.csv(tweets_google_df, file='GOOGL.csv', row.names=F)

tweets_apple <- search_tweets('$AAPL', n=3000, lang='en')
tweets_apple_df <- as.data.frame(tweets_apple)
tweets_apple_df <- apply(tweets_apple_df,2,as.character)
write.csv(tweets_apple_df, file='AAPL.csv', row.names=F)

tweets_amazon <- search_tweets('$AMZN', n=3000, lang='en')
tweets_amazon_df <- as.data.frame(tweets_amazon)
tweets_amazon_df <- apply(tweets_amazon_df,2,as.character)
write.csv(tweets_amazon_df, file='AMZN.csv', row.names=F)

apple <- read.csv(file = 'AAPL.csv', header = T)
google <- read.csv(file = 'GOOGL.csv', header = T)
amazon <- read.csv(file = 'AMZN.csv', header = T)

corpus <- iconv(google$text, to='UTF-8')
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)
removeURL <- function(x) gsub(pattern = "\\@\\w*|\\#\\w*|\\$\\w*|(f|ht)tp(s?)://(.*)|[^\x01-\x7F]", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, gsub, pattern='stocks', replacement='stock')

# Top words in Google Stock Tweets
tdm <- as.matrix(TermDocumentMatrix(corpus))
words <- rowSums(tdm)
words <- subset(words, words>=50)
barplot(words, las=2, col=rainbow(50))

# Word Cloud for Google Stock Tweets
words <- sort(words, decreasing = T)
wordcloud(words = names(words), freq = words, max.words = 30)

words <- data.frame(names(words), words)
colnames(words) <- c('word', 'frequency')
wordcloud2(words, size = 0.5, shape = 'pentagon')

# Sentiment Analysis for Google Stock Tweets
google_tweets <- iconv(google$text, to='UTF-8')
sentiment_scores <- get_nrc_sentiment(google_tweets)
barplot(colSums(sentiment_scores), las = 2, col = rainbow(10), ylab = 'Count', main = 'Sentiment Scores for Google Stocks')

# Sentiment Analysis for Apple Stock Tweets
apple_tweets <- iconv(apple$text, to='UTF-8')
sentiment_scores <- get_nrc_sentiment(apple_tweets)
barplot(colSums(sentiment_scores), las = 2, col = rainbow(10), ylab = 'Count', main = 'Sentiment Scores for Apple Stocks')

# Sentiment Analysis for Amazon Stock Tweets
amazon_tweets <- iconv(amazon$text, to='UTF-8')
sentiment_scores <- get_nrc_sentiment(amazon_tweets)
barplot(colSums(sentiment_scores), las = 2, col = rainbow(10), ylab = 'Count', main = 'Sentiment Scores for Amazon Stocks')
