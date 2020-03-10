
sms_data <- read.csv("~/Downloads/Data Science/data set/sms_raw_NB.csv", stringsAsFactors=F)

class(sms_data)
head(sms_data)
table(sms_data$type)
sms_data$type <- as.factor(sms_data$type)
library(tm) 


sms_corpus <- Corpus(VectorSource(sms_data$text))
class(sms_corpus)
head(sms_corpus$content)

sms_text_clean <- tm_map(sms_corpus,tolower)
sms_text_clean <- tm_map(sms_text_clean,removeNumbers)
sms_text_clean <- tm_map(sms_text_clean,removeWords, stopwords())
sms_text_clean <- tm_map(sms_text_clean,removePunctuation)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","", x)
sms_text_clean <- tm_map(sms_text_clean,content_transformer(removeNumPunct))
sms_text_clean <- tm_map(sms_text_clean,stripWhitespace)
class(sms_text_clean)
sms_text_clean$content[1:10]

sms_dtm <- DocumentTermMatrix(sms_text_clean)

sms_raw_train <- sms_data[1:4169, ]
sms_raw_test  <- sms_data[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- sms_text_clean[1:4169]
sms_corpus_test  <- sms_text_clean[4170:5559]

prop.table(table(sms_train$type))
prop.table(table(sms_test$type))


sms_dict<-findFreqTerms(sms_dtm, 3)
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))


convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}


sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)


sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)


CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,
           prop.r = FALSE, dnn = c('predicted', 'actual'))




