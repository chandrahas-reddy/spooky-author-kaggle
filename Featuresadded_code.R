
install.packages(c("plyr", "dplyr", "ldatuning", "tidyverse", "tidytext", "stringr", "tm", "topicmodels",
                   "ggplot2","caret","readr","text2vec"))


library(plyr)
library(dplyr)
library(ldatuning)
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(topicmodels)
library(ggplot2)
library(caret)
library("readr")
library(text2vec)

#read data
train <- read.csv("train.csv", stringsAsFactors = FALSE)


#topic modeling

my_source<-VectorSource(train$text)
corpus<-Corpus(my_source)
#corpus<-tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
corpus<- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus<- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))

#Remove empty cells and create a new corpus that aligns with the processed dtm
rowTotals <- apply(dtm , 1, sum)
empty.rows<-dtm[rowTotals == 0,]$dimnames[1][[1]]
empty.rows<-as.numeric(empty.rows)
corpus <- corpus[-empty.rows]

#Create a dataframe of the new corpus
corpus.df<-as.data.frame(corpus$content)

#Create the dtm again with the new corpus
dtm <- DocumentTermMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))

#Making sure that the original data set i.e., model.raw aligns with the new corpus. To do that we have to remove the same row numbers in empty.rows in model.raw
x<-length(as.numeric(empty.rows))# calculate the number of empty.rows


#Write a loop that goes through the row numbers of model.raw and delete those rows that match with delete.rows
empty.rows[1]
for (i in 1:x){
  model.raw<-model.raw[-empty.rows[i],]
  i<-i+1
}

#Random check to see consistency between the new and old datasets
corpus.df[743,]
train[743,]$text

#Run the LDA topic model
lda<-LDA(dtm, k=9, control = list(seed=2343))

#Using tidytext manifest topics
topics<-tidy(lda, matrix="beta")
topics

spooky_topics <- topics

spooky_top_terms <- spooky_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

spooky_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + theme_bw()


#bigrams
spooky_bigrams <- train %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

spooky_bigrams

#bigrams
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

count_bigrams(train)

#trigrams
count_trigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word)%>%
    count(word1, word2, word3, sort = TRUE)
}

count_trigrams(train)

#text length
train$TextLength <- nchar(train$text)
View(train)

#comma features
createFeature = function(ds)
{
  ds = ds %>%
    mutate(commas = str_count(ds$text, ",")) %>%
    mutate(semicolumns = str_count(ds$text, ";")) %>%
    mutate(colons = str_count(ds$text, ":"))
  return(ds)
}

train.Features <- createFeature(train)


#sentiment
pos <- readLines("positive_words.txt")
neg <- readLines("negative_words.txt")

spooky_sentiment <- as.data.frame(train.Features$text)
colnames(spooky_sentiment)[1] <- "text"

spooky_sentiment  <- sapply(spooky_sentiment ,function(row) iconv(row, "latin1", "ASCII", sub=""))


#function to calculate sentiment score
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    # split sentence into words with str_split (stringr package)
                    word.list <- str_split(sentence, "\\s+")
                    words <- unlist(word.list)
                    
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches <- match(words, pos)
                    neg.matches <- match(words, neg)
                    
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    
                    # final score
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}
#sentiment score
sentiment_score <- score.sentiment(spooky_sentiment, pos, neg, .progress='text')
summary(sentiment_score)
View(sentiment_score)

#Convert sentiment scores from numeric to character to enable the gsub function 
sentiment_score$sentiment <- as.character(sentiment_score$score)

#After looking at the summary(scores_spNew$sentiment) decide on a threshold for the sentiment labels
sentiment_score$sentiment <- gsub("^0$", "Neutral", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^1$|^2$|^3$|^4$", "Positive", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$|^20$|^21$|^22$|^23$|^24$|^25$", "Very Positive", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", sentiment_score$sentiment)


View(sentiment_score)


#adding sentiment to train.features
train.Features$Sentimentscore <- sentiment_score[,2]
train.Features$SentimentLabel <- sentiment_score[,3]
View(train.Features)

#------------ Stratified split of train data into split.train(70%), split.test(30%)

library(caret)

# Using caret to create a 70%/30% stratified  split. 
# Random seed set for reproducibility.

set.seed(3284)
indexes <- createDataPartition(train.Features$author, times = 1, p = 0.7, list = FALSE)
split.train <- train.Features[indexes,]
split.test <- train.Features[-indexes,]

prop.table(table(split.train$author))
prop.table(table(split.test$author))


View(split.train)
View(split.test)

