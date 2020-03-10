
library("twitteR")

library("ROAuth")

cred <- OAuthFactory$new(consumerKey='KMo58bdjhqXnDbyvzZibisOJF',
                         consumerSecret='jBLtTeTc1hdehbvquA4FxzZZg0VEy2XvK25rKlnPMJvzen3pvT', 
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

library(base64enc)

library(httpuv)


setup_twitter_oauth("KMo58bdjhqXnDbyvzZibisOJF",
                    "jBLtTeTc1hdehbvquA4FxzZZg0VEy2XvK25rKlnPMJvzen3pvT", 
                    "1112341077527150592-rieGeC2xODHswyE97G3Mj1rWcw6XKi", 
                    "aXeHbY4T2e8eJBbHWcYLsf3C3VSCDjWLFvfNCGI1YIJ1B") 

Tweets <- userTimeline('BarackObama', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()

head(Tweets)

library(tm)

myCorpus <- Corpus(VectorSource(TweetsDF$text))

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

myStopwords <- c(setdiff(stopwords('english'), c("r", "big")), "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myCorpus <- tm_map(myCorpus, stripWhitespace)

myCorpusCopy <- myCorpus

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))

tdm

freq.terms <- findFreqTerms(tdm, lowfreq = 10)
freq.terms[1:150]
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 1000)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot2::ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

library(wordcloud)
m <- as.matrix(tdm)

word.freq <- sort(rowSums(m), decreasing = T)

pal <- brewer.pal(9, "BuGn")[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 300,
          random.order = F, colors = pal)


Twitt_reviews <- read.delim('Tweets.csv')
reviews <- as.character(Twitt_reviews[-1,])
class(reviews)
s <- get_nrc_sentiment(reviews)
reviews[4]
get_nrc_sentiment('splendid')
get_nrc_sentiment('no words')

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for BarackObama Twitt')
