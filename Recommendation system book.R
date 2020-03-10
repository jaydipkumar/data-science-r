library(recommenderlab)
library(reshape2)
book_data <- read.csv("~/Downloads/Data Science/data set/books.csv",stringsAsFactors = F)


#EDA

#Data wranggling.
book_data <- book_data[1:2500,]
View(book_data)
book_data <- book_data[,-1]
colnames(book_data) <- c("users","title","author","publisher","ratings")
head(book_data)
str(book_data)
book_data$title <- as.factor(book_data$title)
book_data$author <- as.factor(book_data$author)
book_data$publisher <- as.factor(book_data$publisher)
str(book_data)
attach(book_data)
sum(is.na(book_data))
table(book_data$ratings)

#Plotting.
plot(book_data$users,book_data$ratings,ylab="Ratings",xlab="Users",main="Ratings Distribution")
qqplot(book_data$users,book_data$ratings)
boxplot(book_data$ratings)



author_matrix <- as.matrix(acast(book_data, users~author, fun.aggregate = mean))
publisher_matrix <- as.matrix(acast(book_data, users~publisher, fun.aggregate = mean))
ratings_matrix <- as.matrix(acast(book_data, users~ratings, fun.aggregate = mean))
View(author_matrix)
View(publisher_matrix)
View(ratings_matrix)

author_matrix_r <- as(author_matrix, "realRatingMatrix")
publisher_matrix_r <- as(publisher_matrix, "realRatingMatrix")
ratings_matrix_r <- as(ratings_matrix, "realRatingMatrix")

author_user_based = Recommender(binarize(author_matrix_r,minRating=5), method="UBCF") ## User-based collaborative filtering
author_item_based = Recommender(binarize(author_matrix_r,minRating=5), method="IBCF") ## Item-based collaborative filtering

author_popular = Recommender(binarize(author_matrix_r,minRating=5), method="POPULAR")


publisher_user_based = Recommender(binarize(publisher_matrix_r,minRating=5), method="UBCF") ## User-based collaborative filtering
publisher_item_based = Recommender(binarize(publisher_matrix_r,minRating=5), method="IBCF") ## Item-based collaborative filtering

publisher_popular = Recommender(binarize(publisher_matrix_r,minRating=5), method="POPULAR")

ratings_user_based = Recommender(binarize(ratings_matrix_r,minRating=5), method="UBCF") ## User-based collaborative filtering
ratings_item_based = Recommender(binarize(ratings_matrix_r,minRating=5), method="IBCF") ## Item-based collaborative filtering

ratings_popular = Recommender(binarize(ratings_matrix_r,minRating=5), method="POPULAR")

#Author based recommendation.
#1,10,333
uid = "2" 
print("recommendations for you:")
prediction <- predict(author_user_based, author_matrix_r[uid],n=5) 
author_list <- as(prediction, "list",strict = T)
author_vector <- unlist(author_list)
recommended_book <- book_data[which(book_data$author %in% author_vector), ]
summary(recommended_book)
recommended_book$title

#publisher based recommendation.

uid = "1" #Create Recommendation for user 1 based on author.
print("recommendations for you:")
prediction1 <- predict(publisher_user_based, publisher_matrix_r[uid],n=5) 
publisher_list <- as(prediction1, "list",strict = T)
publisher_vector <- unlist(publisher_list)
recommended_book_1 <- book_data[which(book_data$publisher %in% publisher_vector), ]
summary(recommended_book_1)
recommended_book_1$title #Recommended Book


#Ratings based recommendation.

uid = "1" #Create Recommendation for user 1 based on author.
print("recommendations for you:")
prediction2 <- predict(ratings_user_based, ratings_matrix_r[uid],n=5) 
ratings_list <- as(prediction2, "list",strict = T)
ratings_vector <- unlist(ratings_list)
recommended_book_2 <- book_data[which(book_data$ratings %in% ratings_vector), ]
summary(recommended_book_2)
recommended_book_2$title ##Recommended Book 

