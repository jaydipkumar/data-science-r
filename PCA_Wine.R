
wine <- read.csv("~/Downloads/Data Science/data set/wine.csv")

attach(wine)

wine_scores<-princomp(wine,cor = TRUE,scores = TRUE,covmat = NULL)

summary(wine_scores)

str(wine_scores)

loadings(wine_scores)

plot(wine_scores)

biplot(wine_scores)

plot(cumsum(wine_scores$sdev*wine_scores$sdev)*100/(sum(wine_scores$sdev*wine_scores$sdev)),type="b")

wine_scores$scores[,1:3]

new_wine_data <- cbind(wine,wine_scores$scores[,1:3])

wine_cluster <- new_wine_data[,15:17]

#scale

wine_cluster_scale <- scale(wine_cluster)
wine_euclidean <- dist(wine_cluster_scale,method = "euclidean")
wine_maximum <- dist(wine_cluster_scale,method = "maximum")
wine_manhattan <- dist(wine_cluster_scale,method ="manhattan")

#distance

wine_complete_linkage <- hclust(wine_euclidean,method = "complete")
wine_centroid_linkage <- hclust(wine_maximum,method = "centroid")
wine_mcquitty_linkage <- hclust(wine_manhattan,method = "mcquitty")

#Dendrograme

plot(wine_complete_linkage,labels = FALSE,hang = -1)

rect.hclust(wine_complete_linkage,k=7,border = "blue")

#kmeans

wine_k <- kmeans(wine_cluster_scale,4)
str(wine_k)
total_within = NULL
for (i in 2:10) {
  total_within <- c(total_within,kmeans(wine_cluster_scale,i)$tot.withinss)
  
}

wine_cluster_final <- kmeans(wine_cluster_scale,3)
wine_cluster_final$cluster
wine["PCA_Cluster"] <- wine_cluster_final$cluster
wine<-wine [,c (15,1:14)]
aggregate(wine,by = list(wine$PCA_Cluster),mean)