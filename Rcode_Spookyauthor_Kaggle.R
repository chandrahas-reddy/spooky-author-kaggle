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

