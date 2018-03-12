#Spooky author data - topic modelling

#set working directory

#installing all the necessary packages
install.packages(c("tm", "ggplot2", "caret", "lattice", "quanteda", "tidyr", "topicmodels", "tidytext", "dplyr","ldatuning", "stringr"))

#Loading all the necessary libraries 
library(tm)
library(ggplot2)
library(caret)
library(lattice)
library(quanteda)
library(tidyr)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ldatuning)
library(stringr)

#Reading in the data in the form of a csv file
model.raw<-read.csv("train.csv", stringsAsFactors = FALSE)
View(model.raw)

# Adding a new feature as a new coloumn
model.raw$TextLength<-nchar(model.raw$text)


#model.raw<-model.raw[-2,]
my_source<-VectorSource(model.raw$text)
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
model.raw[743,]$text

#Run the LDA topic model
lda<-LDA(dtm, k=9, control = list(seed=2343))

#Using tidytext manifest topics
topics<-tidy(lda, matrix="beta")
topics

#Showing the top terms and grouping them by topics created - Using dplyr's top_n limiting to 10
top_terms<-topics %>%
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

#Convert top_terms$topic to a factor variable - for visualization 
top_terms$topic<-as.factor(top_terms$topic)

#Visualization of the top terms.
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  theme_bw()+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Creating a per-document-per-topic probability list
documents<-tidy(lda, matrix="gamma")
documents


#Finding the highest probablity gamma score to assign topic number for each document
i<-1
max_pos<-vector()
for(i in documents$document){
  
  x<-filter(documents, document==i)
  max<-max(x$gamma) 
  max_pos[length(max_pos)+1]<-max
  #mutate(documents, max)
  
}

#Merging the per document topic probabilities to model.raw
documents$max<-max_pos
documents$diff<-documents$gamma-documents$max
documents$corpus.df<-corpus.df$Content

documents<-filter(documents, diff==0)
documents$document<-as.numeric(documents$document)
documents<-arrange(documents,document)

model.raw<-cbind(model.raw,documents)



#Export results 

#Finding out how many topics to create
result<-FindTopicsNumber(dtm,
                         topics=seq(from = 2, to =15, by=1),
                         metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                         method = "Gibbs",
                         control = list(seed = 77),
                         mc.cores = 3L,
                         verbose = TRUE)

FindTopicsNumber_plot(result)

