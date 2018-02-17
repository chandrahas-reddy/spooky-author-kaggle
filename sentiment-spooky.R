install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest", "plyr", "stringr"))

library(plyr)
library(stringr)


spooky.train <- read.csv("train.csv", stringsAsFactors = FALSE)
spa <- spooky.train
spooky.test <- read.csv("test.csv", stringsAsFactors = FALSE)

View(spooky.train)
View(spooky.test)

# Check data to see if there are missing values.
length(which(!complete.cases(spooky.train)))

# Convert our class label into a factor.
spa$author <- as.factor(spooky.train$author)


# Next up, the distribution of text lengths of the sentences 
# by adding a new feature for the length of each message.
spa$TextLength <- nchar(spa$text)
View(spa)

pos <- readLines("positive_words.txt")
neg <- readLines("negative_words.txt")

spNew <- as.data.frame(spa$text)
colnames(spNew)[1] <- "clean-text"

spNew <- sapply(spNew,function(row) iconv(row, "latin1", "ASCII", sub=""))

spNew <- gsub("@\\w+", "", spNew)
spNew <- gsub("#\\w+", '', spNew)
spNew <- gsub("RT\\w+", "", spNew)
spNew <- gsub("http.*", "", spNew)
spNew <- gsub("RT", "", spNew)
spNew <- sub("([.-])|[[:punct:]]", "\\1", spNew)
spNew <- sub("(['])|[[:punct:]]", "\\1", spNew)

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
                    # remove punctuation
                    sentence <- gsub("[[:punct:]]", "", sentence)
                    # remove control characters
                    sentence <- gsub("[[:cntrl:]]", "", sentence)
                    # remove digits
                    sentence <- gsub('\\d+', '', sentence)
                    
                    #convert to lower
                    sentence <- tolower(sentence)
                    
                    
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
scores_spNew <- score.sentiment(spNew, pos, neg, .progress='text')
summary(scores_spNew)
View(scores_spNew)

#Convert sentiment scores from numeric to character to enable the gsub function 
scores_spNew$sentiment <- as.character(scores_spNew$score)

#After looking at the summary(scores_spNew$sentiment) decide on a threshold for the sentiment labels
scores_spNew$sentiment <- gsub("^0$", "Neutral", scores_spNew$sentiment)
scores_spNew$sentiment <- gsub("^1$|^2$|^3$|^4$", "Positive", scores_spNew$sentiment)
scores_spNew$sentiment <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$|^20$|^21$|^22$|^23$|^24$|^25$", "Very Positive", scores_spNew$sentiment)
scores_spNew$sentiment <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", scores_spNew$sentiment)
scores_spNew$sentiment <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", scores_spNew$sentiment)


#Text Length after punctuation
scores_spNew$newTextLength <- nchar(as.character(scores_spNew$clean.text))

View(scores_spNew)
