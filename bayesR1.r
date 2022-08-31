# Naïve Bayes with R; text classifier
#library(tidyverse) # Set of packages; ggplot, dplur, tydyr...
#library(tidytext)  # text mining
#library(naivebayes)
library(tm) # text mining
#library(caret) # Training and plotting classification and regression models

library(wordcloud)


# Reading Data
library(readtext)
# Train Data File

Data_train <- readtext(("/home/josemo/Rprogram/dataset/C50/C50train/*"),
                       dvsep = "\n")

head(Data_train$text, n = 1)

# Test Data File
#data_test_dir <- system.file("Data/C50/C50test/")
Data_test <- readtext(("/home/josemo/Rprogram/dataset/C50/C50test/*"),
                      dvsep = "\n")


# Author names
Authornames <- as.data.frame(rep(basename(
  list.dirs("/home/josemo/Rprogram/dataset/C50/C50train")),
  each = 50))
Authornames
Authornames <- Authornames[-(1:50),]
Authornames

# Assigning Author name to Text
Data_test$Author <- Authornames
Data_train$Author <- Authornames

####### 
#Dropping ID Column
Data_test <- Data_test[-1]
Data_train <- Data_train[-1]
####### 


# Converting Author Column to Factor
Data_test$Author <- as.factor(Data_test$Author)
Data_train$Author <- as.factor(Data_train$Author)

# Filtering Data by 4 Authors
AaronTrain <- Data_train %>% filter(Author == "AaronPressman", text == text)
JaneTrain <- Data_train %>% filter(Author == "JaneMacartney", text == text)
SarahTrain <- Data_train %>% filter(Author == "SarahDavison", text == text)
WilliamTrain <- Data_train %>% filter(Author == "WilliamKazer", text == text)
Data_train <- rbind(AaronTrain, JaneTrain, SarahTrain, WilliamTrain)

AaronTest <- Data_test %>% filter(Author == "AaronPressman", text == text)
JaneTest <- Data_test %>% filter(Author == "JaneMacartney", text == text)
SarahTest <- Data_test %>% filter(Author == "SarahDavison", text == text)
WilliamTest <- Data_test %>% filter(Author == "WilliamKazer", text == text)
Data_test <- rbind(AaronTest, JaneTest, SarahTest, WilliamTest)

dim(Data_test)  # 200    2

#Checking for missing values
#any(is.na(data_train))
#any(is.na(data_test))


# Creating Corpus
suppressWarnings(Data_test_corpus <- Corpus(VectorSource(Data_test$text)))
suppressWarnings(Data_train_corpus <- Corpus(VectorSource(Data_train$text)))

# Corpus cleaning
suppressWarnings(Data_test_corpus_clean <- tm_map(Data_test_corpus, tolower))
suppressWarnings(Data_train_corpus_clean <- tm_map(Data_train_corpus, tolower))


suppressWarnings(Data_test_corpus_clean <- tm_map(Data_test_corpus_clean, removeNumbers))
suppressWarnings(Data_train_corpus_clean <- tm_map(Data_train_corpus_clean, removeNumbers))

suppressWarnings(Data_test_corpus_clean <- tm_map(Data_test_corpus_clean, removeWords, stopwords()))
suppressWarnings(Data_train_corpus_clean <- tm_map(Data_train_corpus_clean, removeWords, stopwords()))

suppressWarnings(Data_test_corpus_clean <- tm_map(Data_test_corpus_clean, removePunctuation))
suppressWarnings(Data_train_corpus_clean <- tm_map(Data_train_corpus_clean, removePunctuation))

suppressWarnings(Data_test_corpus_clean <- tm_map(Data_test_corpus_clean, stripWhitespace))
suppressWarnings(Data_train_corpus_clean <- tm_map(Data_train_corpus_clean, stripWhitespace))

suppressWarnings(inspect(Data_train_corpus_clean[1]))
                 
# Word Cloud of Testing Dataset
wordcloud(Data_test_corpus_clean, min.freq = 40, random.order = FALSE)

# Sparse matrix creation; matriz dispersa
test_dtm <- DocumentTermMatrix(Data_test_corpus_clean)
train_dtm <- DocumentTermMatrix(Data_train_corpus_clean)

inspect(train_dtm)

##### Preparing Training and Testing Datasets #####
### Creating Indicator features for frequent words ###

FreqWords <- findFreqTerms(train_dtm,5)

# Saving list using dictionary() function
Dictionary <- function(x) {
  if( is.character(x)) {
    return (x)  
  }
  stop(' x is not a character vector')
}

data_dict <- Dictionary(FreqWords)
#data_dict <- Dictionary(findFreqTerms(train_dtm, 5))

# Appending Document Term Matrix to Train and Test Dataset 
data_train <- DocumentTermMatrix(Data_train_corpus_clean, list(data_dict))
data_test <- DocumentTermMatrix(Data_test_corpus_clean, list(data_dict))


# Converting the frequency of word to count
convert_counts <- function(x) {
  x <- ifelse( x > 0, 1, 0)
  x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
  return (x)
}

# Appending count function to Train and Test Dataset
data_train <- apply(data_train, MARGIN = 2, convert_counts)
data_test <- apply(data_test, MARGIN = 2, convert_counts)


# Training a Model

#Naive Bayes Classification
library(e1071)
data_classifier <- naiveBayes(data_train, Data_train$Author)

# Evaluating model performance
library(gmodels)

# Prediction
data_test_pred <- predict(data_classifier, data_test)

CrossTable(data_test_pred, Data_test$Author,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted','actual'))


#
# Improving model performance
# Setting Laplace = 1
library(e1071)
data_classifier2 <- naiveBayes(data_train, Data_train$Author, laplace=1)
# repeat
# Prediction
data_test_pred2 <- predict(data_classifier2, data_test)

CrossTable(data_test_pred2, Data_test$Author,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted','actual'))


