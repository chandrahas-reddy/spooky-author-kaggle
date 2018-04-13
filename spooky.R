#Kaggle
#Spooky Author Identification


#--------Part-1--------#
# 1. Installing required packages
# 2. Loading packages
# 3. Read data into a dataframe


# 1. Installing required packages----------
install.packages(c("plyr", "dplyr", "ldatuning", "tidyverse", "tidytext", "stringr", "tm", "topicmodels",
                   "ggplot2", "caret","readr","text2vec", "xgboost", "e1071"))

# 2. Loading packages------------
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
library(e1071)
library(xgboost)
library(randomForest)
library(rpart)

# 3. Read train data provided into "train" variable creating a dataframe-----------
train <- read.csv("train.csv", stringsAsFactors = FALSE)

#reading in lexicon positive and negative words
pos <- readLines("positive_words.txt")
neg <- readLines("negative_words.txt")

######################################################################

#--------Part-2--------#
# 1. Writing required functions for analysis
# 2. Data Exploration 
#    - Topic Modeling
#    - creating bigrams
#    - creating trigrams


# 1. Writing required functions for analysis
#function to creare Document term matrix
funcDTM <- function(train) {
  my_source <- VectorSource(train$text)
  corpus<-Corpus(my_source)
  corpus<- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus<- tm_map(corpus, stemDocument)
  
  #Create the dtm again with the new corpus
  dtm <- DocumentTermMatrix(corpus)
  dtm <- removeSparseTerms(dtm, 0.997)
  #Create a dataframe of the new corpus
  spookyLabeledTerms<-as.data.frame(as.matrix(dtm))
  
  return(spookyLabeledTerms)
}

#comma features function
createFeature <- function(ds)
{
  ds = ds %>%
    mutate(commas = str_count(ds$text, ",")) %>%
    mutate(semicolumns = str_count(ds$text, ";")) %>%
    mutate(colons = str_count(ds$text, ":"))
  return(ds)
}

#sentiment analysis function
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

######################################################################

######################################################################

# 2. Data Exploration 
spookyDTM <- funcDTM(train)
spookyDTM <- spookyDTM[rowSums(abs(spookyDTM)) != 0,]

#Run the LDA topic model
lda<-LDA(spookyDTM, k=9, control = list(seed=2343))
lda

#Using tidytext to find the topics
topics<-tidy(lda, matrix="beta")
topics

spooky_topics <- topics

# Visualization 1 : Top 10 terms in each of the 9 topics-----------
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


# Bigrams
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

######################################################################

######################################################################

#--------Part-3--------#
# 1. Adding text length to train 
# 2. Adding comma features to train
# 3. Performing sentiment Analysis and adding sentiment scores to train
# 4. Stratified Split


# 1. Adding text length to train
train$TextLength <- nchar(train$text)

######################################################################

# 2. Adding comma features to train
train <- createFeature(train)

######################################################################

# 3. Sentiment analysis
spooky_sentiment <- as.data.frame(train$text)
colnames(spooky_sentiment)[1] <- "text"

spooky_sentiment  <- sapply(spooky_sentiment ,function(row) iconv(row, "latin1", "ASCII", sub=""))

#sentiment score
sentiment_score <- score.sentiment(spooky_sentiment, pos, neg, .progress='text')
summary(sentiment_score)
View(sentiment_score)

#Convert sentiment scores from numeric to character to enable the gsub function 
sentiment_score$sentiment <- as.character(sentiment_score$score)

#After looking at the summary(sentiment_Score$sentiment) decide on a threshold for the sentiment labels
sentiment_score$sentiment <- gsub("^0$", "Neutral", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^1$|^2$|^3$|^4$", "Positive", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$|^20$|^21$|^22$|^23$|^24$|^25$", "Very Positive", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", sentiment_score$sentiment)

View(sentiment_score)

#adding sentiment to train
train$Sentimentscore <- sentiment_score[,2]
train$SentimentLabel <- sentiment_score[,3]
View(train)


######################################################################

#4. Stratified Split
indexes <- createDataPartition(train$author, times = 1, p = 0.7, list = FALSE)
split.train <- train[indexes,]
split.test <- train[-indexes,]

######################################################################

######################################################################

#--------Part-4--------#
# 1. Preparing model for training
# 2. Adding features to split.train and split.test sets
# 3. Model building using XGBoost
# 4. Predicting in split.test

# 1. Preparing model for training
spookyLabeledTerms<-funcDTM(split.train)
spookyLabeledTerms.Test<-funcDTM(split.test)

colnamesSame = intersect(colnames(spookyLabeledTerms),colnames(spookyLabeledTerms.Test))

spookyLabeledTerms = spookyLabeledTerms[ , (colnames(spookyLabeledTerms) %in% colnamesSame)]
spookyLabeledTerms.Test = spookyLabeledTerms.Test[ , (colnames(spookyLabeledTerms.Test) %in% colnamesSame)]

######################################################################

# 2. Adding features to split.train and split.test sets
spookyLabeledTerms$Length <- split.train$TextLength
spookyLabeledTerms.Test$Length <- split.test$TextLength

spookyLabeledTerms$sentimentscore <- split.train$Sentimentscore
spookyLabeledTerms.Test$sentimentscore <- split.test$Sentimentscore

spookyLabeledTerms$Ncommas <- split.train$commas
spookyLabeledTerms.Test$Ncommas <- split.test$commas

spookyLabeledTerms$Nsemicolons <- split.train$semicolumns
spookyLabeledTerms.Test$Nsemicolons <- split.test$semicolumns

spookyLabeledTerms$Ncolons <- split.train$colons
spookyLabeledTerms.Test$Ncolons <- split.test$colons


spookyLabeledTerms$author = as.factor(split.train$author)
levels(spookyLabeledTerms$author) = make.names(unique(spookyLabeledTerms$author))

######################################################################

# 3. Model building using XGBoost
formula = author ~ .
fitControl <- trainControl(method="none",classProbs=TRUE, summaryFunction=mnLogLoss)

xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 3,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .8,
                       min_child_weight = 1,
                       subsample = 1)


set.seed(13)

AuthorXGB = train(formula, data = spookyLabeledTerms, 
                  method = "xgbTree", trControl = fitControl, 
                  tuneGrid = xgbGrid, na.action = na.pass, 
                  metric="LogLoss", maximize=FALSE)

# getting the importance of variables
importance <- varImp(AuthorXGB)
importance

######################################################################

# 4. Predicting in split.test
# predicting the author value from the spookyLabeledTerms.Test data
yXb_predict <- predict(AuthorXGB, newdata = spookyLabeledTerms.Test)

# creation confusion matrix 
cmXGB = table(split.test[,3], yXb_predict)
cmXGB

varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                            Importance = round(importance[[1]]$Overall,2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance)))) %>%
  head(20)

rankImportancefull <- rankImportance

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = Variables, y = 1, label = Rank),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_bw()

######################################################################

######################################################################

#--------Part-5--------#
# 1. Read test data
# 2. Adding comma features
# 3. Performing sentiment analysis
# 4. Preparing model for training
# 5. Adding features to train and test
# 6. Prediction


# 1. Read test data
test <- read_csv("test.csv")

######################################################################

# 2. Adding comma features
test <- createFeature(test)

######################################################################

# 3. Performing sentiment analysis on train and test
#get the train set sentiment scores from part-3

#sentiment analysis on test
spooky_sentiment <- as.data.frame(test$text)
colnames(spooky_sentiment)[1] <- "text"

spooky_sentiment  <- sapply(spooky_sentiment ,function(row) iconv(row, "latin1", "ASCII", sub=""))

#sentiment score
sentiment_score <- score.sentiment(spooky_sentiment, pos, neg, .progress='text')
summary(sentiment_score)
View(sentiment_score)

#Convert sentiment scores from numeric to character to enable the gsub function 
sentiment_score$sentiment <- as.character(sentiment_score$score)

#After looking at the summary(sentiment_Score$sentiment) decide on a threshold for the sentiment labels
sentiment_score$sentiment <- gsub("^0$", "Neutral", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^1$|^2$|^3$|^4$", "Positive", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$|^20$|^21$|^22$|^23$|^24$|^25$", "Very Positive", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", sentiment_score$sentiment)
sentiment_score$sentiment <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", sentiment_score$sentiment)

View(sentiment_score)

#adding sentiment to test
test$Sentimentscore <- sentiment_score[,2]
test$SentimentLabel <- sentiment_score[,3]
View(test)

######################################################################

# 4. Preparing model for training
spookyLabeledTerms <- funcDTM(train)
spookyLabeledTerms.Test <- funcDTM(test)

colnamesSame = intersect(colnames(spookyLabeledTerms),colnames(spookyLabeledTerms.Test))

spookyLabeledTerms = spookyLabeledTerms[ , (colnames(spookyLabeledTerms) %in% colnamesSame)]
spookyLabeledTerms.Test = spookyLabeledTerms.Test[ , (colnames(spookyLabeledTerms.Test) %in% colnamesSame)]

######################################################################

# 5. Adding features to train and test
spookyLabeledTerms$Length <- train$TextLength
spookyLabeledTerms.Test$Length <- test$TextLength

spookyLabeledTerms$sentimentscore <- train$Sentimentscore
spookyLabeledTerms.Test$sentimentscore <- test$Sentimentscore

spookyLabeledTerms$Ncommas <- train$commas
spookyLabeledTerms.Test$Ncommas <- test$commas

spookyLabeledTerms$Nsemicolons <- train$semicolumns
spookyLabeledTerms.Test$Nsemicolons <- test$semicolumns

spookyLabeledTerms$Ncolons <- train$colons
spookyLabeledTerms.Test$Ncolons <- test$colons

spookyLabeledTerms$author = as.factor(train$author)
levels(spookyLabeledTerms$author) = make.names(unique(spookyLabeledTerms$author))


#prediction
predictions <- predict(AuthorXGB, spookyLabeledTerms.Test, type = 'prob')

#creating solution data frame
spookySolution <- data.frame('id' = test$id, predictions)

#view solution
head(spookySolution, n= 10)

######################################################################

