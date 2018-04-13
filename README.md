# Spooky Author Identification - Kaggle

Kaggle Competition link: https://www.kaggle.com/c/spooky-author-identification

This repository helps Data Analytics/Science Enthusiasts to perform feature engineering and predictive analysis on text data provided by Kaggle.

Data for analysis: https://www.kaggle.com/c/spooky-author-identification/data

#1. Introduction
What is Predictive analysis?
Out of all the available definitions on the web, the definition that caught my attention was provided by SAS firm in their website. Which states: "Predictive analytics is the use of data, statistical algorithms and machine learning techniques to identify the likelihood of future outcomes based on historical data. The goal is to go beyond knowing what has happened to providing a best assessment of what will happen in the future."

*What is the competition about?
This competition can be considered as a classic example for predictive modelling and analysis. Competition wants us to train a model based on the given train set, which contains text from various novels written by three writers Edgar Allen Poe (EPL), Mary Shelley (MWS), HP Lovecraft (HPL). Our final goal here is to predict author name for the test dataset which contains only text from the above mentioned authors in random.

Programming Language: R 

Algorithm used for training: XGBoost

#2. Analysis

**Approach to the solution**

Let's divide our analysis into 5 parts:

###### #--------Part-1--------#
 1. Installing required packages
 2. Loading packages
 3. Read data into a dataframe


###### #--------Part-2--------#
 1. Writing required functions for analysis
 2. Data Exploration 
    - Topic Modeling
    - creating bigrams
    - creating trigrams

###### #--------Part-3--------#
 1. Adding text length to train 
 2. Adding comma features to train
 3. Performing sentiment Analysis and adding sentiment scores to train
 4. Stratified Split

###### #--------Part-4--------#
 1. Preparing model for training
 2. Adding features to split.train and split.test sets
 3. Model building using XGBoost
 4. Predicting in split.test

###### #--------Part-5--------#
 1. Read test data
 2. Adding comma features
 3. Performing sentiment analysis
 4. Preparing model for training
 5. Adding features to train and test
 6. Prediction


##3. A look into the code

*Reading in train and test dataset

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

![alt text](https://raw.githubusercontent.com/chandrahas-reddy/spooky-author-kaggle/chandrahas/readData.PNG)

**Data Exploration**
Performing topic modelling:

*Run the LDA topic model
lda<-LDA(spookyDTM, k=9, control = list(seed=2343))

*Using tidytext to find the topics
topics<-tidy(lda, matrix="beta")

*Visualization of top 10 terms of the 9 topics

![alt text](https://raw.githubusercontent.com/chandrahas-reddy/spooky-author-kaggle/chandrahas/topics.jpeg)

*bigrams

count_bigrams(train)

![alt text](https://raw.githubusercontent.com/chandrahas-reddy/spooky-author-kaggle/chandrahas/bigrams.PNG)

*trigrams

count_trigrams(train)

![alt text](https://raw.githubusercontent.com/chandrahas-reddy/spooky-author-kaggle/chandrahas/trigrams.PNG)

**Feature Engineering**
Feature engineering is the process of using domain knowledge of the data to create features that make machine learning algorithms work.

Features:
1. Text Length
2. Number of Commas
3. Number of Semicolons
4. Number of colons
5. Sentiment Analysis

Look at the spooky.R file for all the code.

##4. XGBoost and Results

XGBoost is an optimized distributed gradient boosting library designed to be highly efficient, flexible and portable. It implements machine learning algorithms under the Gradient Boosting framework. XGBoost provides a parallel tree boosting (also known as GBDT, GBM) that solve many data science problems in a fast and accurate way. The same code runs on major distributed environment (Hadoop, SGE, MPI) and can solve problems beyond billions of examples.

**https://github.com/dmlc/xgboost**

*Result*
