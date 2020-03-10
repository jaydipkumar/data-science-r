
library(rvest)
library(XML)
library(magrittr)

aurl <- "https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber"
amazon_reviews <- NULL


for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
amazon_reviews[1]


library("syuzhet")
amzn_sentiment_bing <- get_sentiment(amazon_reviews, method = "bing")
amzn_sentiment_afinn <- get_sentiment(amazon_reviews, method = "afinn")
amzn_sentiment_nrc <- get_sentiment(amazon_reviews, method = "nrc")

library("sentimentr")
??sentiment
amzn_sentimentr <- sentimentr::sentiment(amazon_reviews)
sentiment_data <-sentimentr::sentiment_attributes(amazon_reviews)

negative_review <- amazon_reviews[amzn_sentiment_bing<0]
most_neg_review <- amazon_reviews[which.min(amzn_sentiment_bing)]


positive_review <- amazon_reviews[amzn_sentiment_bing>0]
most_pos_review <- amazon_reviews[which.max(amzn_sentiment_bing)]

nrc_data <- get_nrc_sentiment(amazon_reviews)
nrc_score_neg_review <- get_nrc_sentiment(most_neg_review)
nrc_score_word <- get_nrc_sentiment('wonderful') #to check individual word.

barplot(sort(colSums(prop.table(nrc_data))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)



