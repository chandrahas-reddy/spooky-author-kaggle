#Introduction:
#Spooky Author Identification - Kaggle project
#By - Chandrahas, Aparna, Prashanth

#set working directory
setwd("C:/Users/aparn/OneDrive/Documents/Learn_Extra/Kaggle_SpookyAuthor/spooky-author-kaggle")
getwd()

#install required packages
install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest"))
      # ggplot2 for visulizations ; e1071 used via caret package; caret depends on 1071; 
      # quanteda: main for doing text analytics
      # irlba: single value decomposition SVD and feature extractions
      # randomforest - for feature engineering



# Load up the training  and test .CSV data and explore in RStudio.
spooky.train <- read.csv("train.csv", stringsAsFactors = FALSE)
spooky.test <- read.csv("test.csv", stringsAsFactors = FALSE)
      #by default R makes the string values as categorical variables. Using stringsAsFactors = FALSE does not take the values as categorical 
View(spooky.train)
View(spooky.test)

# Check data to see if there are missing values.
length(which(!complete.cases(spooky.train)))

# Convert our class label into a factor.
spooky.train$author <- as.factor(spooky.train$author)

# The first step is to explore the data.
# First, let's take a look at distribution of the class labels (i.e., EAP, HPL, MWS)
prop.table(table(spooky.train$author))

# Next up, the distribution of text lengths of the sentences 
# by adding a new feature for the length of each message.
spooky.train$TextLength <- nchar(spooky.train$text)
summary(spooky.train$TextLength)
View(spooky.train)

# Visualize distribution with ggplot2, adding segmentation for authors
library(ggplot2)

ggplot(spooky.train, aes(x = TextLength, fill = author)) + theme_bw() +
  geom_histogram(binwidth = 50) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with Class Labels")


