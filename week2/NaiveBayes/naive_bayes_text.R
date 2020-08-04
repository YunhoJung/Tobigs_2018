# Filtering spam SMS using Naive Bayes

# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8") # to solve the problem when reading csv files encoded in UTF-8 on Window environment with read.csv

# Data Load
sms_raw <-read.csv("sms_spam.csv",stringsAsFactors = FALSE)
head(sms_raw) # labeling text if it's spam or ham spam

## Composed of 5574 messages and two attributes (ham/spam)
## The type variable is stored as character, but need to convert it to factor
## 13.4% -> spam
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
table(sms_raw$type)

## The message consists of strings.
## Since several conjunctions are included, need to handle them, and you should separate sentences into individual words.
## tm package can be the solution -> nlp
## install.packages("tm")
library(tm)

## step1. Build Corpus(말뭉치)
## Corpus refers to a collection of text documents
## VCorpus() -> Create a corpus and store it in memory
## PCorpus() -> Save corpus directly to disk
## The corpus contains documents for 5574 messages.
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus) # 5574

## Need to use as.character() to see the contents of the message inside the corpus.
as.character(sms_corpus[[1]])

## step2. Refine the text inside Corpus
## Map the corpus by using tm_map()
## First, convert all characters to lowercase using tolower()
## and save the mapping result in corpus_clean
sms_corpus_clean<-tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus_clean[[1]])

## Remove numbers using removeNumbers()
sms_corpus_clean<-tm_map(sms_corpus_clean, removeNumbers)


## Remove stop words such as to, and, but, or // Frequently appearing, bu meaningless words
## Frequently appearing, but remove them without regret because it does not provide meaningful information
## If use head, able to see what words are classified as stopwords
head(stopwords(), 20)
length(stopwords()) # The number of stop words : 174
class(stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean, removeWords, stopwords())

## Remove punctuation marks
sms_corpus_clean<-tm_map(sms_corpus_clean, removePunctuation)

## step3. Stemming
## learning, learns, learned -> all converted to learn
## Use wordStem() from the Snowball package
## install.packages("SnowballC")
library(SnowballC)
sms_corpus_clean<-tm_map(sms_corpus_clean, stemDocument)

## remove even meaningless white space
sms_corpus_clean<-tm_map(sms_corpus_clean, stripWhitespace)

## step4. Tokenization
## Input the corpus using DocumentTermMatrix() of the tm package
## The above output comes out as a sparse matrix.
## Row - message, Col - word
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
dim(sms_dtm)
# View(as.matrix(sms_dtm)) # to confirm the contents of sms_dtm


####################################################################
#################### Text Preprocessing Done #######################

## Train/Test Partition - just a naive way without using function
sms_dtm_train<-sms_dtm[1:4169, ]
sms_dtm_test<-sms_dtm[4170:5572, ]

## Store each message separately in a vector to indicate whether it's spam or ham
## Check if the proportion of spam is appropriate in the stored string vector using prop.table
## Good pratition even using a naive way
sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5572,]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

## step5. Word Cloud
## A way to visually describe the frequency of words in text data
## install.packages("wordcloud")
## random.order = FALSE -> This function centers the more frequent words
## min.freq -> This function represents the minimum frequency to appear in the cloud,
## common to set it to about 10% of the corpus
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

## Use the subset() function to divide spam and ham from sms_raw data
## Make a word cloud for the divided data
## Words like call, now, free, mobile, and stop appear in spam mail
## Words like can, get, like, good, got, come, know in ham mail
## This difference would be a keyword to make the NBC model distinguish categories
spam <-subset(sms_raw, type=="spam")
ham <-subset(sms_raw, type=="ham")
wordcloud(spam$text, min.freq = 30, scale=c(3,0,5), random.order = FALSE)
wordcloud(ham$text, min.freq = 30, scale=c(3,0,5), random.order = FALSE)

## step6. Transforming the Structure of the Sparse Matrix
## Change the sparse matrix created above to make it easier for nbc to train
## sms_dtm - 6597 cols
## In order to improve efficiency such as speed, if its frequency is less than 5, the vector will be deleted
## Store the character vector of words in sms_freq_words that appear more than 5 times
## str(sms_freq_words) -> 1158 words
sms_freq_words<-findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

## Extract only the columns in sms_freq_words from the train/test created above
## Then the train/test data now has only 1158 words
sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]

## NBC does not take into account the frequency of words, but the appearance is important
## However, in the sparse matrix, when the word apple appears twice, 2 is stored
## When this happens, NBC can't train
## So, make a function - if the frequency is 0, return 'NO', else return 'YES'(if it is more than 1)
convert_counts<-function(x){
  x<-ifelse(x>0, "Yes","No")
}

## Use the apply function to apply the above functions at once
## MARGIN=2 in the option means that the conver_counts function is applied for each column
sms_train<-apply(sms_dtm_freq_train, MARGIN=2, convert_counts)
sms_test<-apply(sms_dtm_freq_test, MARGIN=2, convert_counts)
summary(sms_train[,1:5])

####################################################################
################### Train/Test Partition Done  #####################

## Train the NB model with pre-processed data
## naiveBayes(train, class, laplace=0)
## train : train data
## class : target vector of train data
## laplace : whether to use Laplace smoothing
library(naivebayes)
sms_classifier<-naive_bayes(sms_train, sms_train_labels, laplace = 1)
head(sms_classifier$table)

## Evaluate the performance of the NB model
## Save the prediction results of the test data in sms_test_pred요
## Accuracy : 97.51%
sms_test_pred<-predict(sms_classifier, sms_test)
library(caret)
confusionMatrix(sms_test_pred, sms_test_labels)